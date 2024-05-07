use std::cmp::Ordering;

use crate::{
    block::{Block, BlockBuilder, BlockKind},
    config::Config,
    svg::SvgShape,
};

pub struct BasicBlock<'a> {
    block_builder: &'a BlockBuilder,
    distance: usize,
    blocks: Vec<Block>,
}

impl<'a> BasicBlock<'a> {
    pub fn new(
        config: &'a Config,
        block_builder: &'a BlockBuilder,
        kind: BlockKind,
        content: String,
    ) -> Self {
        Self {
            block_builder,
            distance: config.distance(),
            blocks: vec![block_builder.build(kind, content)],
        }
    }

    pub fn top_pos(&self) -> (usize, usize) {
        self.blocks.first().unwrap().top_pos()
    }

    pub fn bottom_pos(&self) -> (usize, usize) {
        self.blocks.last().unwrap().bottom_pos()
    }

    pub fn to_svg(&self) -> SvgShape {
        let mut group: Vec<SvgShape> = self.blocks.iter().map(|block| block.to_svg()).collect();
        for block in &self.blocks[..self.blocks.len() - 1] {
            let (x, y) = block.bottom_pos();
            group.push(SvgShape::DownArrow {
                x,
                y,
                height: self.distance,
            })
        }
        SvgShape::Group(group)
    }

    pub fn displace(&mut self, dx: usize, dy: usize) {
        self.blocks
            .iter_mut()
            .for_each(|block| block.displace(dx, dy));
    }

    pub fn push(&mut self, kind: BlockKind, content: String) {
        let (bb_bottom_x, bb_bottom_y) = self.bottom_pos();
        let mut block = self.block_builder.build(kind, content);
        let (block_top_x, _) = block.top_pos();
        match block_top_x.cmp(&bb_bottom_x) {
            Ordering::Less => block.displace(bb_bottom_x - block_top_x, 0),
            Ordering::Greater => self.displace(block_top_x - bb_bottom_x, 0),
            _ => (),
        }
        let dy = bb_bottom_y + self.distance;
        block.displace(0, dy);
        self.blocks.push(block);
    }
}
