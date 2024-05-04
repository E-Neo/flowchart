use crate::{config::Config, svg::SvgShape};
use unicode_width::UnicodeWidthStr;

pub enum BlockKind {
    Terminal,
    IO,
    Process,
    Decision,
}

pub struct Block {
    kind: BlockKind,
    x: usize,
    y: usize,
    width: usize,
    height: usize,
    theta: Option<f64>,
    texts: Vec<(String, usize)>,
}

impl Block {
    pub fn displace(self, dx: usize, dy: usize) -> Block {
        Block {
            x: self.x + dx,
            y: self.y + dy,
            texts: self
                .texts
                .into_iter()
                .map(|(content, cy)| (content, cy + dy))
                .collect(),
            ..self
        }
    }

    pub fn to_svg(&self) -> SvgShape {
        let (x, y) = (self.x, self.y);
        let (width, height) = (self.width, self.height);
        let mut items = self.to_texts();
        items.push(match self.kind {
            BlockKind::Terminal => SvgShape::Stadium {
                x,
                y,
                width,
                height,
            },
            BlockKind::IO => SvgShape::Parallelogram {
                x,
                y,
                theta: self.theta.unwrap(),
                width,
                height,
            },
            BlockKind::Process => SvgShape::Rect {
                x,
                y,
                width,
                height,
            },
            BlockKind::Decision => SvgShape::Diamond {
                x,
                y,
                width,
                height,
            },
        });
        SvgShape::Group(items)
    }

    fn to_texts(&self) -> Vec<SvgShape> {
        let cx = self.x + self.width / 2;
        self.texts
            .iter()
            .map(|(content, cy)| SvgShape::Text {
                cx,
                cy: *cy,
                content: content.clone(),
            })
            .collect()
    }
}

pub struct BlockBuilder {
    grid_size: usize,
    font_size: usize,
    min_width: usize,
    min_height: usize,
    theta: f64,
}

impl BlockBuilder {
    pub fn new(config: &Config) -> Self {
        Self {
            grid_size: config.grid_size(),
            font_size: config.font_size(),
            min_width: config.min_widht(),
            min_height: config.min_height(),
            theta: config.theta(),
        }
    }

    fn estimate_text_width_height(&self, content: &str) -> (usize, usize) {
        let (num_columns, num_lines) = get_num_columns_num_lines(content);
        (
            self.font_size / 2 * num_columns + 2 * self.font_size,
            self.font_size * num_lines + 2 * self.font_size,
        )
    }

    fn fit_to_grid(&self, width: usize, height: usize) -> (usize, usize) {
        let (width, height) = (width.max(self.min_width), height.max(self.min_height));
        (
            width.div_ceil(self.grid_size) * self.grid_size,
            height.div_ceil(self.grid_size) * self.grid_size,
        )
    }

    pub fn build_terminal(&self, content: String) -> Block {
        let (width, height) = self.estimate_text_width_height(&content);
        // The diameter of the circle in the playground is the `height`.
        // And we don't want to write texts in the circle.
        let width = width + height;
        let (width, height) = self.fit_to_grid(width, height);
        Block {
            kind: BlockKind::Terminal,
            x: 0,
            y: 0,
            width,
            height,
            theta: None,
            texts: get_texts(content, height / 2, self.font_size),
        }
    }

    pub fn build_io(&self, content: String) -> Block {
        let (width, height) = self.estimate_text_width_height(&content);
        let width = width + (2.0 * height as f64 / self.theta.tan()).ceil() as usize;
        let (width, height) = self.fit_to_grid(width, height);
        Block {
            kind: BlockKind::IO,
            x: 0,
            y: 0,
            width,
            height,
            theta: Some(self.theta),
            texts: get_texts(content, height / 2, self.font_size),
        }
    }

    pub fn build_process(&self, content: String) -> Block {
        let (width, height) = self.estimate_text_width_height(&content);
        let (width, height) = self.fit_to_grid(width, height);
        Block {
            kind: BlockKind::Process,
            x: 0,
            y: 0,
            width,
            height,
            theta: None,
            texts: get_texts(content, height / 2, self.font_size),
        }
    }

    pub fn build_decision(&self, content: String) -> Block {
        let (width, height) = self.estimate_text_width_height(&content);
        let (width, height) = (2 * width, 2 * height);
        let (width, height) = self.fit_to_grid(width, height);
        Block {
            kind: BlockKind::Decision,
            x: 0,
            y: 0,
            width,
            height,
            theta: None,
            texts: get_texts(content, height / 2, self.font_size),
        }
    }
}

fn get_num_columns_num_lines(content: &str) -> (usize, usize) {
    let mut num_columns = 0;
    let mut num_lines = 0;
    for line in content.lines() {
        num_columns = num_columns.max(UnicodeWidthStr::width(line));
        num_lines += 1;
    }
    (num_columns, num_lines)
}

fn get_texts(content: String, cy: usize, font_size: usize) -> Vec<(String, usize)> {
    let num_lines = content.lines().count();
    let dy = (num_lines as isize - 1) * font_size as isize / 2 - cy as isize;
    content
        .lines()
        .into_iter()
        .zip(0..)
        .map(|(line, i)| (String::from(line), (i * font_size as isize - dy) as usize))
        .collect()
}
