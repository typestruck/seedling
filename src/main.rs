mod types;
mod database;

//until we gather enough data to actually try and predict chat matches, the suggestions work as follows:
// users are scored (once per day) according to several metrics (see database::create_grades) and divided into bins
// users' positions in the same bin are randomly shuffled to create a few suggestion lists
// these suggestion lists are sorted by score, online status before being handed to clients
fn main() {
    let mut client = database::connect();
    let grades = database::create_grades(&mut client);
    let lists = database::create_suggestion_lists(&grades);

    database::update_suggestion_lists(&mut client, lists).expect("Couldn't perform transaction!");
}
