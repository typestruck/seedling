use petgraph::graph::{UnGraph};
use crate::database::Chat;

pub fn create_network(chats: &[Chat]) -> UnGraph::<u32, f64> {
    return UnGraph::from_edges(create_edges(chats));
}

//refactor: check if the graph actually need the edge weight (i.e. can be calculated elsewhere)
fn create_edges(chats: &[Chat]) -> Vec<(u32, u32, f64)> {
    let karmas :Vec<i32> = chats.iter().map(|chat| chat.karma).filter(|&karma| karma > 0).collect();
    let minimum_karma = maybe_or_zero(karmas.iter().min());
    let maximum_karma = maybe_or_zero(karmas.iter().max());

    return chats.iter().map(|chat| (chat.first_user_id, chat.second_user_id, if chat.blocked { -1.0 } else {(chat.karma - minimum_karma) as f64 / (maximum_karma - minimum_karma) as f64})).collect();
}

fn maybe_or_zero(value: Option<&i32>) -> i32 {
    return match value {
        Some(&x) => x,
        None => 0
    };
}

