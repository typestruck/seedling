use crate::types::Grade;
use postgres::{Client, Error, NoTls};

pub fn connect() -> Result<Client, Error> {
    return Client::connect("host=localhost user=melanchat dbname=melanchat", NoTls);
}

pub fn create_grades(client: &mut Client) -> Result<Vec<Grade>, Error> {
    //the "score" of a user is just the sum of a few some metrics, some of which:
    // karma is (linearly) converted to the range 1..255 so it doesn't obscure the other metrics
    // for backers there is a slight score bonus
    let query = "
        select  id,
                cast((raw_score + raw_score * backer_bonus) as integer) score,
                is_new_user
        from    (with k as  (select id,
                                    (select sum(amount) from karmaHistories where target = u.id) karma,
                                    (select count(1) from histories where sender = u.id) chats_started,
                                    (select count(1) from histories where recipient = u.id) chats_accepted,
                                    (select count(1) from blocks where blocked = u.id) bad_chats,
                                    0 time_spent_online,
                                    0 achievements,
                                    0 backer_bonus,
                                    date_part('day', age(now() at time zone 'utc', joined)) < 8 is_new_user
                            from users u
                            order by karma desc)
                select id,
                       is_new_user,
                       ((greatest(karma, 0) * 255 / (select karma from k limit 1)) +
                                chats_started +
                                chats_accepted -
                                bad_chats +
                                time_spent_online +
                                achievements) raw_score,
                        backer_bonus
                from k) t
        order by score desc;";
    let mut grades = Vec::new();

    for row in client.query(query, &[])? {
        grades.push(Grade {
            id: row.get(0),
            score: row.get(1),
            is_new_user: row.get(2),
        })
    }

    return Ok(grades);
}

pub fn create_suggestions(grades: &mut [Grade]) -> String {
    let mut query =
        "insert into suggestions (suggested, score) values".to_owned();
    let total = grades.len();
    let median = grades[total / 2].score;
    //new users (e.g less than a week old) are artificially placed higher
    for gr in grades.iter_mut() {
        if gr.is_new_user {
            gr.score = median
        }
    }
    //sort the collection (since new users might have moded) to avoid order bys
    grades.sort_unstable_by(|s, s2| s2.score.cmp(&s.score));

    for (i, gr) in grades.iter().enumerate() {
        let token = if i == total - 1 { ";" } else { "," };

        query.push_str(&format!(
            "({},{}){}",
            gr.id, gr.score, token
        ));
    }

    return query;
}

pub fn update_suggestions(client: &mut Client, lists: String) -> Result<(), Error> {
    let mut transaction = client.transaction()?;

    //the table is truncated since the suggestions are expired
    transaction.execute("truncate table suggestions;", &[])?;
    transaction.execute(lists.as_str(), &[])?;
    return transaction.commit();
}
