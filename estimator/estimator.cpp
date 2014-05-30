#include <eigen3/Eigen/Core>
//#include <eigen3/Eigen/Cholesky>
#include <eigen3/Eigen/Dense>

#include <cstdio>
#include <iostream>

#include "estmath.hpp"
#include "structs.hpp"
#include "system.hpp"

using namespace std;

Eigen::Matrix<double,6,1> vecOfGps(const gps_t & y) {
    Eigen::Matrix<double,6,1> ret;
    ret << y.r_n2a_n, y.v_an_n;
    return ret;
}

error_state_t errOfVec(const Eigen::Matrix<double,15,1> & v) {
    error_state_t x;
    x.r_n2m_n    = v.segment( 0,3);
    x.v_mn_n     = v.segment( 3,3);
    x.q_m2e      = v.segment( 6,3);
    x.gyro_bias  = v.segment( 9,3);
    x.accel_bias = v.segment(12,3);
    return x;
}

void imu_update(full_state_t & x, const full_state_t & x0, const imu_t & imu, const double dt) {
    Eigen::Matrix<double,15,15> F;
    Eigen::Matrix<double,15,12> G;
    compute_FG(F, G, x0, imu, dt);
    Eigen::Matrix<double,12,12> Q;
    Q = Eigen::Matrix<double,12,12>::Identity(); // todo: correct noise
    x.P = F*x0.P*F.transpose() + G*Q*G.transpose();
    propogate_state(x, x0, imu, dt);
}

void project_error(full_state_t & x, const error_state_t & err) {
    x.r_n2m_n += err.r_n2m_n;
    x.v_mn_n += err.v_mn_n;
    Eigen::Vector4d q_m2e(1, err.q_m2e(0), err.q_m2e(1), err.q_m2e(2));
    x.q_n2m = quat_mult(x.q_n2m, q_m2e).normalized();
    x.gyro_bias += err.gyro_bias;
    x.accel_bias += err.accel_bias;
}

void gps_update(full_state_t & x, const full_state_t & x0, const imu_t & imu, const gps_t & gps) {
    Eigen::Matrix<double,6,15> H;
    gps_t y0;
    expected_gps(y0, imu, x0);
    compute_gps_H(H, x0);
    Eigen::Matrix<double,6,6> R = Eigen::Matrix<double,6,6>::Identity(); // todo: correct noise
    Eigen::Matrix<double,6,6> S = H*x0.P*H.transpose() + R;
    //Eigen::LLT<Eigen::Matrix<double,6,6>> lltS(S);
    //m15_t Sinv = lltS.inverse();
    //Eigen::Matrix<double,6,6> Sinv = S.inverse();
    Eigen::Matrix<double,15,6> K = x0.P*H.transpose()*S.inverse();

    Eigen::Matrix<double,15,1> xerrVec = K*(vecOfGps(y0) - vecOfGps(gps));
    x = x0;
    project_error(x, errOfVec(xerrVec));
}



int main() {
    full_state_t x0 = {.r_n2m_n = Eigen::Vector3d(0,0,0),
                       .v_mn_n = Eigen::Vector3d(0,0,0),
                       .q_n2m = Eigen::Vector4d(1,0,0,0),
                       .gyro_bias = Eigen::Vector3d(0,0,0),
                       .accel_bias = Eigen::Vector3d(0,0,0),
                       .P = Eigen::Matrix<double,15,15>::Identity() // todo: better initialization
                      };
    imu_t imu = {Eigen::Vector3d(1,2,3), Eigen::Vector3d(4,5,6)};
    gps_t gps = {Eigen::Vector3d(1,2,3), Eigen::Vector3d(4,5,6)};
    double dt = 0.01;
    full_state_t x;
    for (int k=0; k<100; k++) {
        imu_update(x, x0, imu, dt);
        x0 = x;
        gps_update(x, x0, imu, gps);
        x0 = x;
        cout << x.P << endl << endl;
    }
    return 0;
}
