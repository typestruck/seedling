use crate::types::Grade;
use postgres::{Client, Error, NoTls};

pub fn connect() -> Result<Client, Error> {
    return Client::connect("host=localhost user=merochat dbname=merochat", NoTls);
}

pub fn create_grades(client: &mut Client) -> Result<Vec<Grade>, Error> {
    //the "score" of a user is just the sum of a few metrics
    // karma is (linearly) converted to the range 1..255 so it doesn't obscure the other metrics
    // users are then put into bins top 2%, top 10%, top 20%, top 35%, top 55%, rest
    let query = "
        select  id,
                cast((raw_score + raw_score * backer_bonus) as integer) score
        from    (with k as  (select id,
                                    (select sum(amount) from karma_histories where target = u.id) karma,
                                    (select count(1) from histories where sender = u.id) chats_started,
                                    (select count(1) from histories where recipient = u.id) chats_accepted,
                                    (select count(1) from blocks where blocked = u.id) bad_chats,
                                    (select count(1) from badges_users where receiver = u.id) achievements,
                                    (case when u.backer then 0.2 else 0 end) backer_bonus,
                                    ((select count(1) from complete_profiles where completer = u.id) * 2) fullness
                            from users u
                            order by karma desc)
                select id,
                       ((greatest(karma, 0) * 255 / (select karma from k limit 1)) +
                                chats_started +
                                chats_accepted -
                                bad_chats +
                                achievements +
                                fullness) raw_score,
                        backer_bonus
                from k) t
        order by score desc;";
    let mut grades = Vec::new();

    for row in client.query(query, &[])? {
        grades.push(Grade {
            id: row.get(0),
            score: row.get(1)
        })
    }

    return Ok(grades);
}

pub fn create_suggestions(grades: &mut [Grade]) -> String {
    let mut query =
        "insert into suggestions (suggested, score, bin) values".to_owned();
    let total = grades.len();

    for (i, gr) in grades.iter().enumerate() {
        let token = if i == total - 1 { ";" } else { "," };
        let percent = (i as f64 / total as f64) * 100.0;
        let bin;

        if percent <= 2.0 {
            bin = 1;
        } else if percent <= 10.0 {
            bin = 2;
        } else if percent <= 20.0 {
            bin = 3;
        } else if percent <= 35.0 {
            bin = 4;
        } else if percent <= 55.0 {
            bin = 5;
        } else {
            bin = 6;
        }

        query.push_str(&format!(
            "({},{},{}){}",
            gr.id, gr.score, bin, token
        ));
    }

    return query;
}

pub fn update_suggestions(client: &mut Client, lists: String) -> Result<(), Error> {
    let mut transaction = client.transaction()?;

    //the table is truncated since the suggestions have expired
    transaction.execute("truncate table suggestions restart identity cascade;", &[])?;
    transaction.execute(lists.as_str(), &[])?;
    return transaction.commit();
}
