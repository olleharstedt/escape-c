/**
 * How escape-c could look.
 *
 * Vim config:
 *  :set filetype=c
 *  :syn keyword cStatement local let
 */

// TODO: Do `local pi = ...` or `local double pi = ...` or `local pi: double = ...`?
local pi = 3.141592653589793
local solar_mass = 4 * pi * pi
local days_per_year = 365.24
local bodies_count = 5;

struct Body {
  double x;
  double y;
  double z;
  double vx;
  double vy;
  double vz;
  double mass;
};

// TODO: Allow alias of body if it doesn't escape.
local bodies = [
  // Sun
  {
    x = 0,
    y = 0,
    z = 0,
    vx = 0,
    vy = 0,
    vz = 0,
    mass = solar_mass
  },
  // Jupiter
  {
    x = 4.84143144246472090e+00,
    y = -1.16032004402742839e+00,
    z = -1.03622044471123109e-01,
    vx = 1.66007664274403694e-03 * days_per_year,
    vy = 7.69901118419740425e-03 * days_per_year,
    vz = -6.90460016972063023e-05 * days_per_year,
    mass = 9.54791938424326609e-04 * solar_mass
  },
  // Saturn
  {
    x = 8.34336671824457987e+00,
    y = 4.12479856412430479e+00,
    z = -4.03523417114321381e-01,
    vx = -2.76742510726862411e-03 * days_per_year,
    vy = 4.99852801234917238e-03 * days_per_year,
    vz = 2.30417297573763929e-05 * days_per_year,
    mass = 2.85885980666130812e-04 * solar_mass
  },
  // Uranus
  {
    x = 1.28943695621391310e+01,
    y = -1.51111514016986312e+01,
    z = -2.23307578892655734e-01,
    vx = 2.96460137564761618e-03 * days_per_year,
    vy = 2.37847173959480950e-03 * days_per_year,
    vz = -2.96589568540237556e-05 * days_per_year,
    mass = 4.36624404335156298e-05 * solar_mass
  },
  // Neptune
  {
    x = 1.53796971148509165e+01,
    y = -2.59193146099879641e+01,
    z = 1.79258772950371181e-01,
    vx = 2.68067772490389322e-03 * days_per_year,
    vy = 1.62824170038242295e-03 * days_per_year,
    vz = -9.51592254519715870e-05 * days_per_year,
    mass = 5.15138902046611451e-05 * solar_mass
  }
];

/**
 * @param &Body[] bodies
 * @param double dt
 * @return void
 */
void advance(&Body[] bodies, double dt) {
  local n = bodies_count - 1;
  for (local i = 0; i < bodies_count - 1; i += 1) {
    for (local j = i + 1; j < bodies_count - 1; j += 1) {
      local dx = bodies[i].x - bodies[j].x;
      local dy = bodies[i].y - bodies[j].y;
      local dz = bodies[i].z - bodies[j].z;
      local dist2 = dx * dx + dy * dy + dz * dz;
      local mag = dt / (dist2 * sqrt(dist2));

      bodies[i].vx = bodies[i].vx - dx * bodies[j].mass * mag;
      bodies[i].vy = bodies[i].vy - dy * bodies[j].mass * mag;
      bodies[i].vz = bodies[i].vz - dz * bodies[j].mass * mag;

      bodies[j].vx = bodies[j].vx + dx * bodies[i].mass * mag;
      bodies[j].vy = bodies[j].vy + dy * bodies[i].mass * mag;
      bodies[j].vz = bodies[j].vz + dz * bodies[i].mass * mag;
    }
  }
  for (local i = 0; i < n; i += 1) {
    bodies[i].x = bodies[i].x + dt * bodies[i].vx;
    bodies[i].y = bodies[i].y + dt * bodies[i].vy;
    bodies[i].z = bodies[i].z + dt * bodies[i].vz;
  }
}

/**
 * @param &Body[] bodies
 * @return double
 */
double energy(&Body[] bodies) {
  local e = 0;
  for (local i = 0; i < bodies_count - 1; i += 1) {
    e = e + 0.5 * bodies[i].mass * (bodies[i].vx * bodies[i].vx + bodies[i].vy * bodies[i].vy + bodies[i].vz * bodies[i].vz);
    for (local j = i + 1; j <  bodies_count - 1; j += 1) {
      local dx = bodies[i].x - bodies[j].x;
      local dy = bodies[i].y - bodies[j].y;
      local dz = bodies[i].z - bodies[j].z;
      local distance = sqrt(dx * dx + dy * dy + dz * dz);
      e = e - (bodies[i].mass * bodies[j].mass) / distance;
    }
  }
  return e;
}

/**
 * @param &Body[] bodies
 * @return void
 */
void offset_momentum(&Body[] bodies) {
  local px = 0;
  local py = 0;
  local pz = 0;
  for (local i = 0; i < bodies_count - 1; i += 1) {
    px = px + bodies[i].vx * bodies[i].mass;
    py = py + bodies[i].vy * bodies[i].mass;
    pz = pz + bodies[i].vz * bodies[i].mass;
  }
  bodies[0].vx = -px / solar_mass;
  bodies[0].vy = -py / solar_mass;
  bodies[0].vz = -pz / solar_mass;
}

int main() {
  local n = 1000;
  offset_momentum(bodies);
  //Printf.printf "%.9f\n" (energy bodies);
  for (local i = 1; i < n; i += 1) {
      advance(bodies, 0.01);
  }
  //Printf.printf "%.9f\n" (energy bodies)
}
