/*
 * Scheduler.h -> Event.h
 *
 *  Created on: 19 May 2020
 *      Author: gc1610
 */

#ifndef INST_INCLUDE_SCHEDULER_H_
#define INST_INCLUDE_SCHEDULER_H_

#include "common_types.h"
#include <Rcpp.h>
#include <set>
#include <map>
#include <functional>

using listener_t = std::function<void (size_t)>;
using targeted_listener_t = std::function<void (size_t, const individual_index_t&)>;

inline std::vector<size_t> round_delay(const std::vector<double>& delay) {
    auto rounded = std::vector<size_t>(delay.size());
    for (auto i = 0u; i < delay.size(); ++i) {
        if (delay[i] < 0) {
            Rcpp::stop("delay must be >= 0");
        }
        rounded[i] = static_cast<size_t>(round(delay[i]));
    }
    return rounded;
}

struct EventBase {
    std::vector<SEXP> listeners;
    size_t t = 0;

    void add_listener(SEXP listener) {
        listeners.push_back(listener);
    }

    virtual void tick() {
        ++t;
    }

    virtual void process() = 0;
};

struct Event : public EventBase {

    std::set<size_t> simple_schedule;

    virtual void process() override {
        if (*simple_schedule.begin() == t) {
            for (const auto& listener : listeners) {
                if (TYPEOF(listener) == EXTPTRSXP) {
                    auto cpp_listener = Rcpp::as<Rcpp::XPtr<listener_t>>(
                        listener
                    );
                    (*cpp_listener)(t);
                } else {
                    Rcpp::Function r_listener = listener;
                    r_listener(t);
                }
            }
        }
    }

    virtual void tick() override {
        simple_schedule.erase(t);
        EventBase::tick();
    }

    void schedule(std::vector<double> delays) {
        for (auto delay : round_delay(delays)) {
            simple_schedule.insert(t + delay);
        }
    }

    void clear_schedule() {
        simple_schedule.clear();
    }
};

struct TargetedEvent : public EventBase {

    std::map<size_t, individual_index_t> targeted_schedule;

    size_t size = 0;

    TargetedEvent(size_t size) : size(size) {};

    virtual void process() override {
        if (targeted_schedule.begin() == targeted_schedule.end()) {
            return;
        }
        if (targeted_schedule.begin()->first == t) {
            const auto& target = targeted_schedule.begin()->second;
            for (const auto& listener : listeners) {
                if (TYPEOF(listener) == EXTPTRSXP) {
                    auto cpp_listener = Rcpp::as<Rcpp::XPtr<
                        targeted_listener_t
                    >>(listener);
                    (*cpp_listener)(t, target);
                } else {
                    Rcpp::Function r_listener = listener;
                    auto r_target = std::vector<size_t>(target.size());
                    auto i = 0;
                    for (auto ti : target) {
                        r_target[i] = ti + 1;
                        ++i;
                    }
                    r_listener(t, r_target);
                }
            }
        }
    }

    virtual void tick() override {
        targeted_schedule.erase(t);
        EventBase::tick();
    }

    //Schedule each individual in `target_vector` to fire an event
    //at a corresponding `delay` timestep in the future.
    //Delays may be continuous but our timeline is discrete.
    //So delays are rounded to the nearest timestep
    void schedule(
        const std::vector<size_t>& target_vector,
        const std::vector<double>& delay) {

        //round the delays to find a discrete timestep to trigger each event
        auto rounded = round_delay(delay);

        //get unique timesteps
        auto delay_values = std::unordered_set<size_t>(
            rounded.begin(),
            rounded.end()
        );

        for (auto v : delay_values) {
            auto target = individual_index_t(size);
            for (auto i = 0u; i < rounded.size(); ++i) {
                if (rounded[i] == v) {
                    target.insert(target_vector[i]);
                }
            }
            schedule(target, v);
        }
    }

    void schedule(
        const individual_index_t& target,
        double delay) {
        schedule(target, static_cast<size_t>(round(delay)));
    }

    void schedule(
        const individual_index_t& target,
        size_t delay) {

        auto target_timestep = t + delay;
        if (targeted_schedule.find(target_timestep) == targeted_schedule.end()) {
            targeted_schedule.insert(
                {target_timestep, individual_index_t(size)}
            );
        }
        targeted_schedule.at(target_timestep) |= target;
    }

    void clear_schedule(const individual_index_t& target) {
        auto not_target = ~target;
        for (auto& entry : targeted_schedule) {
           entry.second &= not_target;
        }
    }

    individual_index_t get_scheduled() const {
        auto scheduled = individual_index_t(size);
        for (auto& entry : targeted_schedule) {
           scheduled |= entry.second;
        }
        return scheduled;
    }
};

#endif /* INST_INCLUDE_SCHEDULER_H_ */
