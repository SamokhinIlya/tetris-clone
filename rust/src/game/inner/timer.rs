pub struct Timer {
    timespan: f64,
    current: f64,
}

impl Timer {
    pub fn new(timespan: f64) -> Self {
        Self {
            timespan,
            current: timespan,
        }
    }

    pub fn tick(&mut self, dt: f64) -> bool {
        self.current -= dt;
        if self.current < 0.0 {
            self.current = self.timespan;
            true
        } else {
            false
        }
    }
}