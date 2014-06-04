#pragma once

typedef struct {
    Eigen::Vector3d gyros;
    Eigen::Vector3d accel;
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
} imu_t;

typedef struct {
    Eigen::Vector3d r_n2a_n;
    Eigen::Vector3d v_an_n;
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
} gps_t;

typedef struct {
    Eigen::Vector3d r_n2m_n;
    Eigen::Vector3d v_mn_n;
    Eigen::Vector3d q_m2e;
    Eigen::Vector3d gyro_bias;
    Eigen::Vector3d accel_bias;
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
} error_state_t;

typedef struct {
    Eigen::Vector3d r_n2m_n;
    Eigen::Vector3d v_mn_n;
    Eigen::Vector4d q_n2m;
    Eigen::Vector3d gyro_bias;
    Eigen::Vector3d accel_bias;
    Eigen::Matrix<double,15,15> P;
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
} full_state_t;
