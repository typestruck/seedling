
mod database;
mod network;
mod suggestions;
mod classifier;

fn main() {
    let users = database::create_users();
    let chat_network = database::create_chats();
    let first_user = &users[0];
    let suggestions_for_user = suggestions::create_suggestions(first_user, &chat_network, &users);

    for (key, value) in &suggestions_for_user {
        println!("{:?}", key);
        println!("{:?}", value);
    }
}
