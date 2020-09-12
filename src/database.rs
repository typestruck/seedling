use crate::types::Grade;
use postgres::{Client, Error, NoTls};
use rand::distributions::{Distribution, Uniform};

pub fn connect() -> Result<Client, Error> {
    return Client::connect("host=localhost user=melanchat dbname=melanchat", NoTls);
}

pub fn create_grades(client: &mut Client) -> Result<Vec<Grade>, Error> {
    //the "score" of a user is just the sum of a few some metrics, some of which:
    // karma is (linearly) converted to the range 1..255 so it doesn't obscure the other metrics
    // for backers there is a slight score bonus
    let query = "
        select  id,
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
                            order by karma)
                select id,
                       is_new_user,
                       ((greatest(karma, 0) / (select karma from k limit 1) * 255) +
                                chats_started +
                                chats_accepted -
                                bad_chats +
                                time_spent_online +
                                achievements) raw_score,
                        backer_bonus
                from k) t
        order by (t.raw_score + t.raw_score * t.backer_bonus) desc;";
    let mut grades = Vec::new();

    for row in client.query(query, &[])? {
        grades.push(Grade {
            id: row.get(0),
            is_new_user: row.get(1),
        })
    }

    return Ok(grades);
}

pub fn create_suggestion_lists(grades: &[Grade]) -> String {
    let mut rng = rand::thread_rng();
    //guess rather than actual binning:
    // the user grades are divided into 5 categories, from terrible to excellent
    // new users (ie less than one week old accounts) are always neutral
    let bins = grades.len() as u32 / 5;
    let mut query =
        "insert into suggestions (suggested, listA, listB, listC, listD, listE) values".to_owned();

    for (i, gr) in grades.iter().enumerate() {
        let iplus = i + 1;
        let position = if gr.is_new_user {
            3
        } else {
            (iplus as f32 / bins as f32).ceil() as u32
        };

        let between = Uniform::new_inclusive((position - 1) * bins, position * bins);
        let list_a = iplus;
        let list_b = between.sample(&mut rng);
        let list_c = between.sample(&mut rng);
        let list_d = between.sample(&mut rng);
        let list_e = between.sample(&mut rng);
        let token = if i == grades.len() - 1 { ";" } else { "," };

        query.push_str(&format!(
            "({},{},{},{},{},{}){}",
            gr.id, list_a, list_b, list_c, list_d, list_e, token
        ));
    }

    return query;
}

pub fn update_suggestion_lists(client: &mut Client, lists: String) -> Result<(), Error> {
    let mut transaction = client.transaction()?;

    //the table is truncated since the suggestions are expired
    transaction.execute("truncate table suggestions;", &[])?;
    transaction.execute(lists.as_str(), &[])?;
    return transaction.commit();
}
