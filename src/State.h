/*
 * Simulation.h
 *
 *  Created on: 3 Mar 2020
 *      Author: giovanni
 */

#ifndef SRC_STATE_H_
#define SRC_STATE_H_

#include <Rcpp.h>
#include <queue>
#include "types.h"

using variable_spec_t = std::pair<std::string, std::vector<double>>;
using state_spec_t = std::pair<std::string, size_t>;
using individual_spec_t = std::tuple<std::string, std::vector<state_spec_t>, std::vector<variable_spec_t>>;
using sim_state_spec_t = std::vector<individual_spec_t>;

class State {
    states_t states;
    variables_t variables;
    std::vector<std::string> individual_names;
    named_array_t<std::vector<std::string>> variable_names;
    named_array_t<size_t> population_sizes;
    std::queue<state_update_t> state_update_queue;
    std::queue<variable_update_t> variable_update_queue;
    void apply_state_update(const state_update_t&);
    void apply_variable_update(const variable_update_t&);
public:
    State(const sim_state_spec_t&);
    void apply_updates();
    const individual_index_t get_state(const std::string, const std::vector<std::string>) const;
    const variable_vector_t& get_variable(const std::string, const std::string) const;
    void queue_state_update(const std::string, const std::string, const individual_index_t&);
    void queue_variable_update(const std::string, const std::string, const std::vector<size_t>&, const variable_vector_t&);
};

#endif
