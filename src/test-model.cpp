#include "categorical_variable.h"
#include "bitset.h"
#include "event.h"

//N <- 1e3
size_t N = 1e3;
//I0 <- 5
size_t I0 = 5;
// S0 <- N - I0
size_t S0 = N - I0;
//dt <- 0.1
double dt = 0.1;
//tmax <- 100
size_t tmax = 100;
//steps <- tmax / dt
size_t steps = tmax /dt;
// gamma <- 1 / 10
double gamma = 0.1;
//R0 <- 2.5
double R0 = 2.5; //
//beta <- R0 * gamma
double beta = R0 * gamma;
std::vector<std::string> health_states = {"S", "I", "R"}
// health_states_t0 <- rep("S", N)
std::vector<std::string> health_states_t0 = std::vector<std::string>(N)
//health_states_t0[sample.int(n = N, size = I0)] <- "I"

//health <- CategoricalVariable$new(categories = health_states,
//                                  initial_values = health_states_t0)
//

auto health = create_categorical_variable(health_states, health_states_t0);

//infection_process <- function(t) {
//  I <- health$get_size_of("I")
//  foi <- beta * I / N
//  S <- health$get_index_of("S")
//  S$sample(rate = pexp(q = foi * dt))
//  health$queue_update(value = "I", index = S)
//}

void infection_process(double t) {
    auto I = health.get_size_of("I");
    auto foi = beta * I / N;
    Bitset S = health.get_index_of("S");
    S->sample();
    health->queue_update("I", *S)
}

//recovery_event <- TargetedEvent$new(population_size = N)
auto recovery_event = create_targeted_event(N);
//recovery_event$add_listener(function(t, target) {
//  health$queue_update("R", target)
//})

recovery_process <- function(t) {
  I <- health$get_index_of("I")
  already_scheduled <- recovery_event$get_scheduled()
  I$and(already_scheduled$not(inplace = TRUE))
  rec_times <- rgeom(n = I$size(), prob = pexp(q = gamma * dt)) + 1
  recovery_event$schedule(target = I, delay = rec_times)
}

health_render <- Render$new(timesteps = steps)
health_render_process <- categorical_count_renderer_process(
  renderer = health_render,
  variable = health,
  categories = health_states
)

simulation_loop(
  variables = list(health),
  events = list(recovery_event),
  processes = list(infection_process, recovery_process, health_render_process),
  timesteps = steps
)

states <- health_render$to_dataframe()
health_cols <- c("royalblue3", "firebrick3", "darkorchid3")
matplot(
  x = states[[1]] * dt, y = states[-1],
  type = "l", lwd = 2, lty = 1, col = adjustcolor(col = health_cols, alpha.f = 0.85),
  xlab = "Time", ylab = "Count"
)
legend(
  x = "topright", pch = rep(16, 3),
  col = health_cols, bg = "transparent",
  legend = health_states, cex = 1.5
)