pub struct Config {
    grid_size: usize,
    font_size: usize,
    min_width: usize,
    min_height: usize,
    theta: f64,
}

impl Config {
    pub fn grid_size(&self) -> usize {
        self.grid_size
    }

    pub fn font_size(&self) -> usize {
        self.font_size
    }

    pub fn min_widht(&self) -> usize {
        self.min_width
    }

    pub fn min_height(&self) -> usize {
        self.min_height
    }

    pub fn theta(&self) -> f64 {
        self.theta
    }
}

pub struct ConfigBuilder {
    config: Config,
}

impl ConfigBuilder {
    pub fn new() -> Self {
        Self {
            config: Config {
                grid_size: 20,
                font_size: 12,
                min_width: 200,
                min_height: 40,
                theta: 1.25,
            },
        }
    }

    pub fn build(self) -> Config {
        self.config
    }

    pub fn grid_size(self, grid_size: usize) -> Self {
        Self {
            config: Config {
                grid_size,
                ..self.config
            },
        }
    }

    pub fn font_size(self, font_size: usize) -> Self {
        Self {
            config: Config {
                font_size,
                ..self.config
            },
        }
    }

    pub fn min_width(self, min_width: usize) -> Self {
        Self {
            config: Config {
                min_width,
                ..self.config
            },
        }
    }

    pub fn min_height(self, min_height: usize) -> Self {
        Self {
            config: Config {
                min_height,
                ..self.config
            },
        }
    }

    pub fn theta(self, theta: f64) -> Self {
        Self {
            config: Config {
                theta,
                ..self.config
            },
        }
    }
}
