#pragma once

#include <eigen3/Eigen/Core>
#include "structs.hpp"

void compute_FG(Eigen::Matrix<double, 15, 15> & F, Eigen::Matrix<double, 15, 12> & G,
                const full_state_t & x, const imu_t & obs, const double dt);

void propogate_state(full_state_t & x, const full_state_t & x0, const imu_t & imu, const double dt);

void compute_gps_H(Eigen::Matrix<double, 6, 15> & H, const full_state_t & x);

void expected_gps(gps_t y, const imu_t & imu, const full_state_t & x);
