use flowchart::{
    block::BlockBuilder,
    config::ConfigBuilder,
    svg::{Svg, SvgShape},
};

fn main() {
    let config = ConfigBuilder::new().grid_size(50).build();
    let mut svg = Svg::new(&config);
    svg.push_shape(SvgShape::Grid {
        size: config.grid_size(),
        x_count: 20,
        y_count: 10,
    });
    let builder = BlockBuilder::new(&config);
    svg.push_shape(
        builder
            .build_terminal(String::from(
                "This is the start blah blah\nThis is the start blah blah",
            ))
            .displace(config.grid_size(), config.grid_size())
            .to_svg(),
    );
    svg.push_shape(
        builder
            .build_io(String::from("Input blah blah blah blah"))
            .displace(config.grid_size(), 3 * config.grid_size())
            .to_svg(),
    );
    svg.push_shape(
        builder
            .build_decision(String::from("blah blah blah blah\nblah blah blah blah"))
            .displace(config.grid_size(), 6 * config.grid_size())
            .to_svg(),
    );
    svg.push_shape(
        builder
            .build_process(String::from("i = 0\nj = 0"))
            .displace(config.grid_size(), 9 * config.grid_size())
            .to_svg(),
    );
    print!("{}", svg);
}
