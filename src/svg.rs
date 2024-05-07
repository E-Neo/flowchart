use std::collections::BTreeMap;

use crate::config::Config;

pub enum SvgShape {
    Group(Vec<SvgShape>),
    Grid {
        size: usize,
        x_count: usize,
        y_count: usize,
    },
    HLine {
        x: usize,
        y: usize,
        width: usize,
    },
    VLine {
        x: usize,
        y: usize,
        height: usize,
    },
    Polyline(Vec<(usize, usize)>),
    Rect {
        x: usize,
        y: usize,
        width: usize,
        height: usize,
    },
    Diamond {
        x: usize,
        y: usize,
        width: usize,
        height: usize,
    },
    Parallelogram {
        x: usize,
        y: usize,
        theta: f64,
        width: usize,
        height: usize,
    },
    Stadium {
        x: usize,
        y: usize,
        width: usize,
        height: usize,
    },
    DownArrow {
        x: usize,
        y: usize,
        height: usize,
    },
    Circle {
        cx: usize,
        cy: usize,
        r: usize,
    },
    Text {
        cx: usize,
        cy: usize,
        content: String,
    },
}

pub struct Svg {
    style: BTreeMap<String, BTreeMap<String, String>>,
    shapes: Vec<SvgShape>,
}

impl Svg {
    pub fn new(config: &Config) -> Self {
        Self {
            style: vec![
                (
                    "rect",
                    vec![("fill", "none"), ("stroke", "black"), ("stroke-width", "1")],
                ),
                ("line", vec![("stroke", "black"), ("stroke-width", "1")]),
                (
                    "polyline",
                    vec![
                        ("fill", "none"),
                        ("stroke", "black"),
                        ("stroke-width", "1"),
                        ("marker-end", "url(#arrow)"),
                    ],
                ),
                (
                    "polygon",
                    vec![("fill", "none"), ("stroke", "black"), ("stroke-width", "1")],
                ),
                (
                    "text",
                    vec![
                        ("font-family", "monospace"),
                        ("font-size", format!("{}px", config.font_size()).as_str()),
                        ("font-size-adjust", "0.5"),
                        ("text-anchor", "middle"),
                        ("dominant-baseline", "middle"),
                    ],
                ),
                ("tspan", vec![("alignment-baseline", "central")]),
                (".grid", vec![("stroke", "yellow"), ("stroke-width", "1")]),
            ]
            .into_iter()
            .map(|(selector, declarations)| {
                (
                    String::from(selector),
                    declarations
                        .into_iter()
                        .map(|(property, value)| (String::from(property), String::from(value)))
                        .collect(),
                )
            })
            .collect(),
            shapes: vec![],
        }
    }

    pub fn push_shape(&mut self, shape: SvgShape) {
        self.shapes.push(shape);
    }
}

macro_rules! write_indent {
    ($dst:expr, $indent:expr, $($arg:tt)*) => {{
        for _ in 0..$indent {
            write!($dst, "  ")?;
        }
        write!($dst, $($arg)*)}
    };
}

macro_rules! writeln_indent {
    ($dst:expr, $indent:expr, $($arg:tt)*) => {{
        for _ in 0..$indent {
            write!($dst, "  ")?;
        }
        writeln!($dst, $($arg)*)}
    };
}

fn write_style(
    f: &mut std::fmt::Formatter<'_>,
    style: &BTreeMap<String, BTreeMap<String, String>>,
) -> std::fmt::Result {
    writeln_indent!(f, 1, "<style>")?;
    for (selector, declarations) in style {
        writeln_indent!(f, 2, "{} {{", selector)?;
        for (property, value) in declarations {
            writeln_indent!(f, 3, "{}: {};", property, value)?;
        }
        writeln_indent!(f, 2, "}}")?;
    }
    writeln_indent!(f, 1, "</style>")?;
    Ok(())
}

fn write_defs(f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    writeln_indent!(f, 1, "<defs>")?;
    writeln_indent!(
        f,
        2,
        r#"<marker id="arrow" viewBox="0 0 10 10" {} orient="auto-start-reverse">"#,
        r#"refX="10" refY="5" markerWidth="6" markerHeight="6""#
    )?;
    writeln_indent!(f, 3, r#"<path d="M 0 0 L 10 5 L 0 10 z" />"#)?;
    writeln_indent!(f, 2, "</marker>")?;
    writeln_indent!(f, 1, "</defs>")
}

fn write_shape(
    f: &mut std::fmt::Formatter<'_>,
    indent: usize,
    shape: &SvgShape,
) -> std::fmt::Result {
    match shape {
        SvgShape::Group(children) => {
            writeln_indent!(f, indent, "<g>")?;
            for child in children {
                write_shape(f, indent + 1, child)?;
            }
            writeln_indent!(f, indent, "</g>")
        }
        SvgShape::Grid {
            size,
            x_count,
            y_count,
        } => {
            let width = size * x_count;
            let height = size * y_count;
            writeln_indent!(f, indent, "<g>")?;
            for x in 0..=*x_count {
                writeln_indent!(
                    f,
                    indent + 1,
                    r#"<line class="grid" x1="{}" y1="{}" x2="{}" y2="{}" />"#,
                    x * size,
                    0,
                    x * size,
                    height
                )?;
            }
            for y in 0..=*y_count {
                writeln_indent!(
                    f,
                    indent + 1,
                    r#"<line class="grid" x1="{}" y1="{}" x2="{}" y2="{}" />"#,
                    0,
                    y * size,
                    width,
                    y * size
                )?;
            }
            writeln_indent!(f, indent, "</g>")?;
            Ok(())
        }
        SvgShape::HLine { x, y, width } => writeln_indent!(
            f,
            indent,
            r#"<line x1="{}" y1="{}" x2="{}" y2="{}" />"#,
            x,
            y,
            x + width,
            y
        ),
        SvgShape::VLine { x, y, height } => writeln_indent!(
            f,
            indent,
            r#"<line x1="{}" y1="{}" x2="{}" y2="{}" />"#,
            x,
            y,
            x,
            y + height
        ),
        SvgShape::Polyline(points) => {
            write_indent!(f, indent, "<polyline points=\"")?;
            match points.as_slice() {
                [] => (),
                [(x, y), tail @ ..] => {
                    write!(f, "{},{}", x, y)?;
                    for (x, y) in tail {
                        write!(f, " {},{}", x, y)?;
                    }
                }
            }
            writeln!(f, "\" />")
        }
        SvgShape::Rect {
            x,
            y,
            width,
            height,
        } => writeln_indent!(
            f,
            indent,
            r#"<rect x="{}" y="{}" width="{}" height="{}" />"#,
            x,
            y,
            width,
            height
        ),
        SvgShape::Diamond {
            x,
            y,
            width,
            height,
        } => writeln_indent!(
            f,
            indent,
            r#"<polygon points="{},{} {},{} {},{} {},{}" />"#,
            x,
            y + height / 2,
            x + width / 2,
            y + height,
            x + width,
            y + height / 2,
            x + width / 2,
            y
        ),
        SvgShape::Parallelogram {
            x,
            y,
            theta,
            width,
            height,
        } => {
            let d = (*height as f64 / theta.tan()) as usize;
            writeln_indent!(
                f,
                indent,
                r#"<polygon points="{},{} {},{} {},{} {},{}" />"#,
                x,
                y + height,
                x + width - d,
                y + height,
                x + width,
                y,
                x + d,
                y
            )
        }
        SvgShape::Stadium {
            x,
            y,
            width,
            height,
        } => writeln_indent!(
            f,
            indent,
            r#"<rect rx="{}" x="{}" y="{}" width="{}" height="{}" />"#,
            height / 2,
            x,
            y,
            width,
            height
        ),
        SvgShape::DownArrow { x, y, height } => {
            writeln_indent!(
                f,
                indent,
                r#"<line x1="{}" y1="{}" x2="{}" y2="{}" marker-end="url(#arrow)"/>"#,
                x,
                y,
                x,
                y + height
            )
        }
        SvgShape::Circle { cx, cy, r } => {
            writeln_indent!(f, indent, r#"<circle cx="{}" cy="{}" r="{}" />"#, cx, cy, r)
        }
        SvgShape::Text { cx, cy, content } => {
            writeln_indent!(
                f,
                indent,
                r#"<text x="{}" y="{}">{}</text>"#,
                cx,
                cy,
                content
            )
        }
    }
}

fn write_shapes(f: &mut std::fmt::Formatter<'_>, shapes: &[SvgShape]) -> std::fmt::Result {
    for shape in shapes {
        write_shape(f, 1, shape)?;
    }
    Ok(())
}

impl std::fmt::Display for Svg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            r#"<?xml version="1.0" encoding="UTF-8" standalone="no"?>"#
        )?;
        writeln!(
            f,
            r#"<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">"#
        )?;
        write_style(f, &self.style)?;
        write_defs(f)?;
        write_shapes(f, &self.shapes)?;
        writeln!(f, "</svg>")
    }
}
