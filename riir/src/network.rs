use petgraph::graph::{UnGraph};
use crate::database::Chat;

pub fn create_network(chats: Vec<Chat>) -> UnGraph::<u32, i32> {
    return UnGraph::<u32, i32>::from_edges(create_edges(chats));
}

fn create_edges(chats: Vec<Chat>) -> Vec<(u32, u32, i32)> {
    let karmas :Vec<i32> = chats.iter().map(|chat| chat.karma).filter(|&karma| karma > 0).collect();
    let minimum_karma = maybe_or_zero(karmas.iter().min());
    let maximum_karma = maybe_or_zero(karmas.iter().max());

    return chats.iter().map(|chat| (chat.first_user, chat.second_user, if chat.blocked { -1 } else {(chat.karma - minimum_karma) / (maximum_karma - minimum_karma)})).collect();
}

fn maybe_or_zero(value: Option<&i32>) -> i32 {
    return match value {
        Some(&x) => x,
        None => 0
    };
}

