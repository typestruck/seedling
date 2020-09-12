use postgres::{Error};

mod types;
mod database;

//until we gather enough data to actually try and predict chat matches, the suggestions work as follows:
// users are scored (once per day) according to several metrics (see database::create_grades) and divided into bins
// these scores are sorted, paginated, then sorted again by online status and random() before being handed to clients
fn main() {
    println!("Rebuilding...");
    rebuild_suggestions().expect("Couldn't rebuild suggestions!");
    println!("Successfully rebuilt suggestions");
}

fn rebuild_suggestions() -> Result<(), Error> {
    let mut client = database::connect()?; //poor man's monad
    let mut grades = database::create_grades(&mut client)?;
    let lists = database::create_suggestions(&mut grades);

    return database::update_suggestions(&mut client, lists);
}
