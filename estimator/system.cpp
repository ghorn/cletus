#include <eigen3/Eigen/Core>
//#include <eigen3/Eigen/Cholesky>
#include <eigen3/Eigen/Dense>

#include "structs.hpp"
#include "estmath.hpp"

#define sq(x) x*x

#define g 9.81
#define tauG  25.0
#define tauA 300.0
static const Eigen::Vector3d r_m2a_m(0, 0, 0);

void compute_FG(Eigen::Matrix<double, 15, 15> & F, Eigen::Matrix<double, 15, 12> & G,
               const full_state_t & x, const imu_t & obs, const double dt) {
    double q0 = x.q_n2m(0);
    double q1 = x.q_n2m(1);
    double q2 = x.q_n2m(2);
    double q3 = x.q_n2m(3);

    double acc0 = obs.accel(0);
    double acc1 = obs.accel(1);
    double acc2 = obs.accel(2);

    double ab0 = x.accel_bias(0);
    double ab1 = x.accel_bias(1);
    double ab2 = x.accel_bias(2);

//    double gb0 = x.gyro_bias(0);
//    double gb1 = x.gyro_bias(1);
//    double gb2 = x.gyro_bias(2);

    // state transfer matrix
    F = Eigen::Matrix<double,15,15>::Constant(0);
    F(0, 0) = 1;
    F(1, 1) = 1;
    F(2, 2) = 1;
    F(0, 3) = dt;
    F(3, 3) = 1;
    F(1, 4) = dt;
    F(4, 4) = 1;
    F(2, 5) = dt;
    F(5, 5) = 1;
    F(0, 6) = ((0.5*sq(dt))*((((acc0-ab0)*(((((q0+q0)*(-q1))+((q1+q1)*q0))-((q2+q2)*q3))-((q3+q3)*(-q2))))+((acc1-ab1)*(2*(((q2*q0)+(q1*q3))-((q3*(-q1))+(q0*(-q2)))))))+((acc2-ab2)*(2*(((q3*q0)+(q1*(-q2)))+((q2*(-q1))+(q0*q3)))))));
    F(1, 6) = ((0.5*sq(dt))*((((acc0-ab0)*(2*(((q2*q0)+(q1*q3))+((q3*(-q1))+(q0*(-q2))))))+((acc1-ab1)*(((((q0+q0)*(-q1))-((q1+q1)*q0))+((q2+q2)*q3))-((q3+q3)*(-q2)))))+((acc2-ab2)*(2*((sq(q3)+(q2*(-q2)))-((q1*(-q1))+sq(q0)))))));
    F(2, 6) = ((0.5*sq(dt))*((((acc0-ab0)*(2*(((q3*q0)+(q1*(-q2)))-((q2*(-q1))+(q0*q3)))))+((acc1-ab1)*(2*((sq(q3)+(q2*(-q2)))+((q1*(-q1))+sq(q0))))))+((acc2-ab2)*(((((q0+q0)*(-q1))-((q1+q1)*q0))-((q2+q2)*q3))+((q3+q3)*(-q2))))));
    F(3, 6) = (dt*((((acc0-ab0)*(((((q0+q0)*(-q1))+((q1+q1)*q0))-((q2+q2)*q3))-((q3+q3)*(-q2))))+((acc1-ab1)*(2*(((q2*q0)+(q1*q3))-((q3*(-q1))+(q0*(-q2)))))))+((acc2-ab2)*(2*(((q3*q0)+(q1*(-q2)))+((q2*(-q1))+(q0*q3)))))));
    F(4, 6) = (dt*((((acc0-ab0)*(2*(((q2*q0)+(q1*q3))+((q3*(-q1))+(q0*(-q2))))))+((acc1-ab1)*(((((q0+q0)*(-q1))-((q1+q1)*q0))+((q2+q2)*q3))-((q3+q3)*(-q2)))))+((acc2-ab2)*(2*((sq(q3)+(q2*(-q2)))-((q1*(-q1))+sq(q0)))))));
    F(5, 6) = (dt*((((acc0-ab0)*(2*(((q3*q0)+(q1*(-q2)))-((q2*(-q1))+(q0*q3)))))+((acc1-ab1)*(2*((sq(q3)+(q2*(-q2)))+((q1*(-q1))+sq(q0))))))+((acc2-ab2)*(((((q0+q0)*(-q1))-((q1+q1)*q0))-((q2+q2)*q3))+((q3+q3)*(-q2))))));
    F(6, 6) = 1;
    F(0, 7) = ((0.5*sq(dt))*((((acc0-ab0)*(((((q0+q0)*(-q2))+((q1+q1)*(-q3)))-((q2+q2)*q0))-((q3+q3)*q1)))+((acc1-ab1)*(2*(((q2*(-q3))+(q1*q0))-((q3*(-q2))+(q0*q1))))))+((acc2-ab2)*(2*(((q3*(-q3))+sq(q1))+((q2*(-q2))+sq(q0)))))));
    F(1, 7) = ((0.5*sq(dt))*((((acc0-ab0)*(2*(((q2*(-q3))+(q1*q0))+((q3*(-q2))+(q0*q1)))))+((acc1-ab1)*(((((q0+q0)*(-q2))-((q1+q1)*(-q3)))+((q2+q2)*q0))-((q3+q3)*q1))))+((acc2-ab2)*(2*(((q3*q0)+(q2*q1))-((q1*(-q2))+(q0*(-q3))))))));
    F(2, 7) = ((0.5*sq(dt))*((((acc0-ab0)*(2*(((q3*(-q3))+sq(q1))-((q2*(-q2))+sq(q0)))))+((acc1-ab1)*(2*(((q3*q0)+(q2*q1))+((q1*(-q2))+(q0*(-q3)))))))+((acc2-ab2)*(((((q0+q0)*(-q2))-((q1+q1)*(-q3)))-((q2+q2)*q0))+((q3+q3)*q1)))));
    F(3, 7) = (dt*((((acc0-ab0)*(((((q0+q0)*(-q2))+((q1+q1)*(-q3)))-((q2+q2)*q0))-((q3+q3)*q1)))+((acc1-ab1)*(2*(((q2*(-q3))+(q1*q0))-((q3*(-q2))+(q0*q1))))))+((acc2-ab2)*(2*(((q3*(-q3))+sq(q1))+((q2*(-q2))+sq(q0)))))));
    F(4, 7) = (dt*((((acc0-ab0)*(2*(((q2*(-q3))+(q1*q0))+((q3*(-q2))+(q0*q1)))))+((acc1-ab1)*(((((q0+q0)*(-q2))-((q1+q1)*(-q3)))+((q2+q2)*q0))-((q3+q3)*q1))))+((acc2-ab2)*(2*(((q3*q0)+(q2*q1))-((q1*(-q2))+(q0*(-q3))))))));
    F(5, 7) = (dt*((((acc0-ab0)*(2*(((q3*(-q3))+sq(q1))-((q2*(-q2))+sq(q0)))))+((acc1-ab1)*(2*(((q3*q0)+(q2*q1))+((q1*(-q2))+(q0*(-q3)))))))+((acc2-ab2)*(((((q0+q0)*(-q2))-((q1+q1)*(-q3)))-((q2+q2)*q0))+((q3+q3)*q1)))));
    F(7, 7) = 1;
    F(0, 8) = ((0.5*sq(dt))*((((acc0-ab0)*(((((q0+q0)*(-q3))+((q1+q1)*q2))-((q2+q2)*(-q1)))-((q3+q3)*q0)))+((acc1-ab1)*(2*((sq(q2)+(q1*(-q1)))-((q3*(-q3))+sq(q0))))))+((acc2-ab2)*(2*(((q3*q2)+(q1*q0))+((q2*(-q3))+(q0*(-q1))))))));
    F(1, 8) = ((0.5*sq(dt))*((((acc0-ab0)*(2*((sq(q2)+(q1*(-q1)))+((q3*(-q3))+sq(q0)))))+((acc1-ab1)*(((((q0+q0)*(-q3))-((q1+q1)*q2))+((q2+q2)*(-q1)))-((q3+q3)*q0))))+((acc2-ab2)*(2*(((q3*(-q1))+(q2*q0))-((q1*(-q3))+(q0*q2)))))));
    F(2, 8) = ((0.5*sq(dt))*((((acc0-ab0)*(2*(((q3*q2)+(q1*q0))-((q2*(-q3))+(q0*(-q1))))))+((acc1-ab1)*(2*(((q3*(-q1))+(q2*q0))+((q1*(-q3))+(q0*q2))))))+((acc2-ab2)*(((((q0+q0)*(-q3))-((q1+q1)*q2))-((q2+q2)*(-q1)))+((q3+q3)*q0)))));
    F(3, 8) = (dt*((((acc0-ab0)*(((((q0+q0)*(-q3))+((q1+q1)*q2))-((q2+q2)*(-q1)))-((q3+q3)*q0)))+((acc1-ab1)*(2*((sq(q2)+(q1*(-q1)))-((q3*(-q3))+sq(q0))))))+((acc2-ab2)*(2*(((q3*q2)+(q1*q0))+((q2*(-q3))+(q0*(-q1))))))));
    F(4, 8) = (dt*((((acc0-ab0)*(2*((sq(q2)+(q1*(-q1)))+((q3*(-q3))+sq(q0)))))+((acc1-ab1)*(((((q0+q0)*(-q3))-((q1+q1)*q2))+((q2+q2)*(-q1)))-((q3+q3)*q0))))+((acc2-ab2)*(2*(((q3*(-q1))+(q2*q0))-((q1*(-q3))+(q0*q2)))))));
    F(5, 8) = (dt*((((acc0-ab0)*(2*(((q3*q2)+(q1*q0))-((q2*(-q3))+(q0*(-q1))))))+((acc1-ab1)*(2*(((q3*(-q1))+(q2*q0))+((q1*(-q3))+(q0*q2))))))+((acc2-ab2)*(((((q0+q0)*(-q3))-((q1+q1)*q2))-((q2+q2)*(-q1)))+((q3+q3)*q0)))));
    F(8, 8) = 1;
    F(6, 9) = (-(0.5*dt));
    F(9, 9) = exp((-(dt/tauG)));
    F(7, 10) = (-(0.5*dt));
    F(10, 10) = exp((-(dt/tauG)));
    F(8, 11) = (-(0.5*dt));
    F(11, 11) = exp((-(dt/tauG)));
    F(0, 12) = ((0.5*sq(dt))*(-(((sq(q0)+sq(q1))-sq(q2))-sq(q3))));
    F(1, 12) = ((0.5*sq(dt))*(-(2*((q1*q2)+(q0*q3)))));
    F(2, 12) = ((0.5*sq(dt))*(-(2*((q1*q3)-(q0*q2)))));
    F(3, 12) = (dt*(-(((sq(q0)+sq(q1))-sq(q2))-sq(q3))));
    F(4, 12) = (dt*(-(2*((q1*q2)+(q0*q3)))));
    F(5, 12) = (dt*(-(2*((q1*q3)-(q0*q2)))));
    F(12, 12) = exp((-(dt/tauA)));
    F(0, 13) = ((0.5*sq(dt))*(-(2*((q1*q2)-(q0*q3)))));
    F(1, 13) = ((0.5*sq(dt))*(-(((sq(q0)-sq(q1))+sq(q2))-sq(q3))));
    F(2, 13) = ((0.5*sq(dt))*(-(2*((q2*q3)+(q0*q1)))));
    F(3, 13) = (dt*(-(2*((q1*q2)-(q0*q3)))));
    F(4, 13) = (dt*(-(((sq(q0)-sq(q1))+sq(q2))-sq(q3))));
    F(5, 13) = (dt*(-(2*((q2*q3)+(q0*q1)))));
    F(13, 13) = exp((-(dt/tauA)));
    F(0, 14) = ((0.5*sq(dt))*(-(2*((q1*q3)+(q0*q2)))));
    F(1, 14) = ((0.5*sq(dt))*(-(2*((q2*q3)-(q0*q1)))));
    F(2, 14) = ((0.5*sq(dt))*(-(((sq(q0)-sq(q1))-sq(q2))+sq(q3))));
    F(3, 14) = (dt*(-(2*((q1*q3)+(q0*q2)))));
    F(4, 14) = (dt*(-(2*((q2*q3)-(q0*q1)))));
    F(5, 14) = (dt*(-(((sq(q0)-sq(q1))-sq(q2))+sq(q3))));
    F(14, 14) = exp((-(dt/tauA)));

    // gyro/accel/bias noise to state
    G = Eigen::Matrix<double,15,12>::Constant(0);
    G(6, 0) = (0.5*dt);
    G(7, 1) = (0.5*dt);
    G(8, 2) = (0.5*dt);
    G(0, 3) = ((0.5*sq(dt))*(((sq(q0)+sq(q1))-sq(q2))-sq(q3)));
    G(1, 3) = ((0.5*sq(dt))*(2*((q1*q2)+(q0*q3))));
    G(2, 3) = ((0.5*sq(dt))*(2*((q1*q3)-(q0*q2))));
    G(3, 3) = (dt*(((sq(q0)+sq(q1))-sq(q2))-sq(q3)));
    G(4, 3) = (dt*(2*((q1*q2)+(q0*q3))));
    G(5, 3) = (dt*(2*((q1*q3)-(q0*q2))));
    G(0, 4) = ((0.5*sq(dt))*(2*((q1*q2)-(q0*q3))));
    G(1, 4) = ((0.5*sq(dt))*(((sq(q0)-sq(q1))+sq(q2))-sq(q3)));
    G(2, 4) = ((0.5*sq(dt))*(2*((q2*q3)+(q0*q1))));
    G(3, 4) = (dt*(2*((q1*q2)-(q0*q3))));
    G(4, 4) = (dt*(((sq(q0)-sq(q1))+sq(q2))-sq(q3)));
    G(5, 4) = (dt*(2*((q2*q3)+(q0*q1))));
    G(0, 5) = ((0.5*sq(dt))*(2*((q1*q3)+(q0*q2))));
    G(1, 5) = ((0.5*sq(dt))*(2*((q2*q3)-(q0*q1))));
    G(2, 5) = ((0.5*sq(dt))*(((sq(q0)-sq(q1))-sq(q2))+sq(q3)));
    G(3, 5) = (dt*(2*((q1*q3)+(q0*q2))));
    G(4, 5) = (dt*(2*((q2*q3)-(q0*q1))));
    G(5, 5) = (dt*(((sq(q0)-sq(q1))-sq(q2))+sq(q3)));
    G(9, 6) = 1;
    G(10, 7) = 1;
    G(11, 8) = 1;
    G(12, 9) = 1;
    G(13, 10) = 1;
    G(14, 11) = 1;
}

void compute_gps_H(Eigen::Matrix<double, 6, 15> & H, const full_state_t & x) {
    double q0 = x.q_n2m(0);
    double q1 = x.q_n2m(1);
    double q2 = x.q_n2m(2);
    double q3 = x.q_n2m(3);
    double r_m2a_m_0 = r_m2a_m(0);
    double r_m2a_m_1 = r_m2a_m(1);
    double r_m2a_m_2 = r_m2a_m(2);

    // state transfer matrix
    H = Eigen::Matrix<double,6,15>::Constant(0);
    H(0, 0) = 1;
    H(1, 1) = 1;
    H(2, 2) = 1;
    H(3, 3) = 1;
    H(4, 4) = 1;
    H(5, 5) = 1;
    H(0, 6) = ((((q2*(-(((q1*(2*r_m2a_m_2))+(q0*(-(2*r_m2a_m_1))))+((q3+q3)*(-r_m2a_m_0)))))+(q3*(((q0*(2*r_m2a_m_2))+(q1*(2*r_m2a_m_1)))+((q2+q2)*(-r_m2a_m_0)))))+(q0*(((q3*(2*r_m2a_m_2))+(q2*(2*r_m2a_m_1)))+((q1+q1)*r_m2a_m_0))))+(q1*(-(((q2*(2*r_m2a_m_2))+(q3*(-(2*r_m2a_m_1))))+((q0+q0)*r_m2a_m_0)))));
    H(1, 6) = ((((q2*(-(((q2*(2*r_m2a_m_2))+((q3+q3)*(-r_m2a_m_1)))+(q0*(2*r_m2a_m_0)))))+(q3*(((q3*(2*r_m2a_m_2))+((q2+q2)*r_m2a_m_1))+(q1*(2*r_m2a_m_0)))))+(q0*(((q0*(-(2*r_m2a_m_2)))+((q1+q1)*(-r_m2a_m_1)))+(q2*(2*r_m2a_m_0)))))+(q1*(-(((q1*(-(2*r_m2a_m_2)))+((q0+q0)*r_m2a_m_1))+(q3*(2*r_m2a_m_0))))));
    H(2, 6) = ((((q2*(-((((q3+q3)*r_m2a_m_2)+(q2*(2*r_m2a_m_1)))+(q1*(2*r_m2a_m_0)))))+(q3*((((q2+q2)*(-r_m2a_m_2))+(q3*(2*r_m2a_m_1)))+(q0*(-(2*r_m2a_m_0))))))+(q0*((((q1+q1)*(-r_m2a_m_2))+(q0*(2*r_m2a_m_1)))+(q3*(2*r_m2a_m_0)))))+(q1*(-((((q0+q0)*r_m2a_m_2)+(q1*(2*r_m2a_m_1)))+(q2*(-(2*r_m2a_m_0)))))));
    H(0, 7) = ((((q1*(((q1*(2*r_m2a_m_2))+(q0*(-(2*r_m2a_m_1))))+((q3+q3)*(-r_m2a_m_0))))+(q0*(((q0*(2*r_m2a_m_2))+(q1*(2*r_m2a_m_1)))+((q2+q2)*(-r_m2a_m_0)))))+(q3*(-(((q3*(2*r_m2a_m_2))+(q2*(2*r_m2a_m_1)))+((q1+q1)*r_m2a_m_0)))))+(q2*(-(((q2*(2*r_m2a_m_2))+(q3*(-(2*r_m2a_m_1))))+((q0+q0)*r_m2a_m_0)))));
    H(1, 7) = ((((q1*(((q2*(2*r_m2a_m_2))+((q3+q3)*(-r_m2a_m_1)))+(q0*(2*r_m2a_m_0))))+(q0*(((q3*(2*r_m2a_m_2))+((q2+q2)*r_m2a_m_1))+(q1*(2*r_m2a_m_0)))))+(q3*(-(((q0*(-(2*r_m2a_m_2)))+((q1+q1)*(-r_m2a_m_1)))+(q2*(2*r_m2a_m_0))))))+(q2*(-(((q1*(-(2*r_m2a_m_2)))+((q0+q0)*r_m2a_m_1))+(q3*(2*r_m2a_m_0))))));
    H(2, 7) = ((((q1*((((q3+q3)*r_m2a_m_2)+(q2*(2*r_m2a_m_1)))+(q1*(2*r_m2a_m_0))))+(q0*((((q2+q2)*(-r_m2a_m_2))+(q3*(2*r_m2a_m_1)))+(q0*(-(2*r_m2a_m_0))))))+(q3*(-((((q1+q1)*(-r_m2a_m_2))+(q0*(2*r_m2a_m_1)))+(q3*(2*r_m2a_m_0))))))+(q2*(-((((q0+q0)*r_m2a_m_2)+(q1*(2*r_m2a_m_1)))+(q2*(-(2*r_m2a_m_0)))))));
    H(0, 8) = ((((q0*(((q1*(2*r_m2a_m_2))+(q0*(-(2*r_m2a_m_1))))+((q3+q3)*(-r_m2a_m_0))))+(q1*(-(((q0*(2*r_m2a_m_2))+(q1*(2*r_m2a_m_1)))+((q2+q2)*(-r_m2a_m_0))))))+(q2*(((q3*(2*r_m2a_m_2))+(q2*(2*r_m2a_m_1)))+((q1+q1)*r_m2a_m_0))))+(q3*(-(((q2*(2*r_m2a_m_2))+(q3*(-(2*r_m2a_m_1))))+((q0+q0)*r_m2a_m_0)))));
    H(1, 8) = ((((q0*(((q2*(2*r_m2a_m_2))+((q3+q3)*(-r_m2a_m_1)))+(q0*(2*r_m2a_m_0))))+(q1*(-(((q3*(2*r_m2a_m_2))+((q2+q2)*r_m2a_m_1))+(q1*(2*r_m2a_m_0))))))+(q2*(((q0*(-(2*r_m2a_m_2)))+((q1+q1)*(-r_m2a_m_1)))+(q2*(2*r_m2a_m_0)))))+(q3*(-(((q1*(-(2*r_m2a_m_2)))+((q0+q0)*r_m2a_m_1))+(q3*(2*r_m2a_m_0))))));
    H(2, 8) = ((((q0*((((q3+q3)*r_m2a_m_2)+(q2*(2*r_m2a_m_1)))+(q1*(2*r_m2a_m_0))))+(q1*(-((((q2+q2)*(-r_m2a_m_2))+(q3*(2*r_m2a_m_1)))+(q0*(-(2*r_m2a_m_0)))))))+(q2*((((q1+q1)*(-r_m2a_m_2))+(q0*(2*r_m2a_m_1)))+(q3*(2*r_m2a_m_0)))))+(q3*(-((((q0+q0)*r_m2a_m_2)+(q1*(2*r_m2a_m_1)))+(q2*(-(2*r_m2a_m_0)))))));
    H(4, 9) = r_m2a_m_2;
    H(5, 9) = (-r_m2a_m_1);
    H(3, 10) = (-r_m2a_m_2);
    H(5, 10) = r_m2a_m_0;
    H(3, 11) = r_m2a_m_1;
    H(4, 11) = (-r_m2a_m_0);
}

void propogate_state(full_state_t & x, const full_state_t & x0, const imu_t & imu, const double dt) {
    Eigen::Vector3d w_mn_m = imu.gyros - x0.gyro_bias;
    Eigen::Matrix3d dcm_n2m = dcm_of_quat(x.q_n2m);
    Eigen::Vector3d a_mn_n = dcm_n2m.transpose() * (imu.accel - x0.accel_bias) - Eigen::Vector3d(0,0,g);

    double w2 = 0.5*w_mn_m.norm();
    Eigen::Vector3d wn = w_mn_m.normalized();

    Eigen::Vector4d q_m02m(cos(w2), wn(0)*sin(w2), wn(1)*sin(w2), wn(2)*sin(w2));
    x.r_n2m_n = x0.r_n2m_n + x0.v_mn_n*dt + 0.5*dt*dt*a_mn_n;
    x.v_mn_n = x0.v_mn_n + a_mn_n*dt;
    x.q_n2m = quat_mult(x0.q_n2m, q_m02m);
    x.gyro_bias = x0.gyro_bias*exp(-dt/tauG);
    x.accel_bias = x0.accel_bias*exp(-dt/tauA);
}

void expected_gps(gps_t y, const imu_t & imu, const full_state_t & x) {
    Eigen::Vector3d w_mn_m = imu.gyros - x.gyro_bias;

    Eigen::Matrix3d dcm_n2m = dcm_of_quat(x.q_n2m);
    Eigen::Vector3d r_m2a_n = dcm_n2m * r_m2a_m;

    y.r_n2a_n = x.r_n2m_n + r_m2a_n;
    y.v_an_n = x.v_mn_n + w_mn_m.cross(r_m2a_m);
}
