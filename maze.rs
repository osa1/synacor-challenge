#![feature(alloc_system)]
extern crate alloc_system;

use std::collections::VecDeque;

/// A tile in the maze
#[derive(Copy, Clone)]
enum Tile {
    Add, Sub, Mul, Num(u32),
}

impl Tile {
    fn oper(&self) -> Option<Oper> {
        match *self {
            Tile::Add => Some(Oper::Add),
            Tile::Sub => Some(Oper::Sub),
            Tile::Mul => Some(Oper::Mul),
            _         => None,
        }
    }

    fn num(&self) -> Option<u32> {
        match *self {
            Tile::Num(n) => Some(n),
            _            => None,
        }
    }
}

/// The maze
static MAZE : [ [ Tile; 4 ]; 4 ] =
    [ [ Tile::Mul,     Tile::Num(8), Tile::Sub,     Tile::Num(1)  ],
      [ Tile::Num(4),  Tile::Mul,    Tile::Num(11), Tile::Mul     ],
      [ Tile::Add,     Tile::Num(4), Tile::Sub,     Tile::Num(18) ],
      [ Tile::Num(22), Tile::Sub,    Tile::Num(9),  Tile::Mul     ] ];

#[derive(Clone, Copy, Debug)]
enum Oper { Add, Sub, Mul }

impl Oper {
    fn apply(&self, n1 : u32, n2 : u32) -> u32 {
        match *self {
            Oper::Add => n1 + n2,
            Oper::Sub => n1 - n2,
            Oper::Mul => n1 * n2,
        }
    }
}

#[derive(Debug)]
struct State {
    /// East-west direction
    pos_x: usize,

    /// North-south direction
    pos_y: usize,

    /// What operator to apply when we see the next number
    oper: Oper,

    /// Total so far
    total: u32,

    /// Moves we've done so far
    moves: Vec<Move>,
}

#[derive(Debug, Copy, Clone)]
enum Move {
    North, East, South, West
}

impl State {
    fn reached_goal(&self) -> bool {
        self.pos_x == 3 && self.pos_y == 0 && self.total == 30
    }
}

impl State {
    fn tile_update(&self, tile : Tile) -> (Oper, u32) {
        let next_oper  = tile.oper().unwrap_or(self.oper);
        let next_score = match tile.num() {
            Some(n) => self.oper.apply(self.total, n),
            None    => self.total
        };

        (next_oper, next_score)
    }

    fn goto(&self, mov : Move) -> State {
        let (pos_x, pos_y) = match mov {
            Move::South => (self.pos_x, self.pos_y + 1),
            Move::North => (self.pos_x, self.pos_y - 1),
            Move::East  => (self.pos_x + 1, self.pos_y),
            Move::West  => (self.pos_x - 1, self.pos_y),
        };

        let tile = MAZE[pos_y][pos_x];
        let (next_oper, next_total) = self.tile_update(tile);
        let mut moves = self.moves.clone();
        moves.push(mov);

        State {
            pos_x: pos_x,
            pos_y: pos_y,
            oper: next_oper,
            total: next_total,
            moves: moves,
        }
    }

    fn north(&self) -> Option<State> {
        if self.pos_y > 0 {
            Some(self.goto(Move::North))
        } else {
            None // can't go north
        }
    }

    fn south(&self) -> Option<State> {
        // We don't ever want to go back to first tile because that resets
        // everything.
        if self.pos_y == 2 && self.pos_x == 0 {
            return None;
        }

        if self.pos_y < 3 {
            Some(self.goto(Move::South))
        } else {
            None // can't go south
        }
    }

    fn west(&self) -> Option<State> {
        // We don't ever want to go back to first tile because that resets
        // everything.
        if self.pos_y == 3 && self.pos_x == 1 {
            return None;
        }

        if self.pos_x > 0 {
            Some(self.goto(Move::West))
        } else {
            None // can't go west
        }
    }

    fn east(&self) -> Option<State> {
        if self.pos_x < 3 {
            Some(self.goto(Move::East))
        } else {
            None // can't go east
        }
    }
}

impl State {
    fn next(&self) -> Vec<State> {
        // It turns out we can't visit the goal tile more than once (the orb
        // disappears).
        if self.pos_x == 3 && self.pos_y == 0 {
            return vec![];
        }

        // Otherwise generate next steps

        let mut ret = Vec::with_capacity(4);

        self.north().map(|s| ret.push(s));
        self.south().map(|s| ret.push(s));
        self.east().map(|s| ret.push(s));
        self.west().map(|s| ret.push(s));

        ret
    }
}

fn main() {
    let initial_state = State {
        pos_x: 0,
        pos_y: 3,
        oper: Oper::Add, // doesn't matter
        total: 22,
        moves: Vec::new(),
    };

    let mut work_queue : VecDeque<State> = VecDeque::new();
    work_queue.push_front(initial_state);

    while let Some(state) = work_queue.pop_back() {
        if state.reached_goal() {
            println!("Found a solution: {:?}", state);
            break;
        }

        for next_state in state.next() {
            work_queue.push_front(next_state);
        }
    }
}
