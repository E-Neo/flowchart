use flowchart::{
    basic_block::BasicBlock,
    block::{BlockBuilder, BlockKind},
    config::ConfigBuilder,
    svg::{Svg, SvgShape},
};

fn main() {
    let config = ConfigBuilder::new().font_size(20).build();
    let mut svg = Svg::new(&config);
    svg.push_shape(SvgShape::Grid {
        size: config.grid_size(),
        x_count: 20,
        y_count: 10,
    });
    let builder = BlockBuilder::new(&config);
    let mut bb = BasicBlock::new(&config, &builder, BlockKind::IO, String::from("input"));
    bb.push(BlockKind::Process, String::from("01234567890123456789"));
    bb.push(BlockKind::Process, String::from("ABCDEFGHIJABCDEFGHIJ"));
    bb.push(BlockKind::Process, String::from("中文中文中文中文中文"));
    bb.push(BlockKind::Decision, String::from("i < 0"));
    bb.displace(config.grid_size(), config.grid_size());
    svg.push_shape(bb.to_svg());
    print!("{}", svg);
}
