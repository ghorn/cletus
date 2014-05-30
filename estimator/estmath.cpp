#include <eigen3/Eigen/Core>
#include <eigen3/Eigen/Dense>

#include "estmath.hpp"

Eigen::Vector4d quat_mult(const Eigen::Vector4d &q, const Eigen::Vector4d &p) {
    double q0 = q(0);
    double p0 = p(0);
    Eigen::Vector3d qv(q(1), q(2), q(3));
    Eigen::Vector3d pv(p(1), p(2), p(3));

    double ret0 = q0*p0 - qv.dot(pv);
    Eigen::Vector3d retv = qv.cross(pv) + q0*pv + p0*qv;

    return Eigen::Vector4d(ret0, retv(0), retv(1), retv(2));
}

Eigen::Matrix3d dcm_of_quat(const Eigen::Vector4d &q) {
    Eigen::Matrix3d dcm;
    double q0 = q(0);
    double q1 = q(1);
    double q2 = q(2);
    double q3 = q(3);

    // 1st column
    dcm(0,0) = q0*q0 + q1*q1 - q2*q2 - q3*q3;
    dcm(1,0) = 2*(q1*q2 - q0*q3);
    dcm(2,0) = 2*(q1*q3 + q0*q2);

    // 2nd column
    dcm(0,1) = 2*(q1*q2 + q0*q3);
    dcm(1,1) = q0*q0 - q1*q1 + q2*q2 - q3*q3;
    dcm(2,1) = 2*(q2*q3 - q0*q1);

    // 3rd column
    dcm(0,2) = 2*(q1*q3 - q0*q2);
    dcm(1,2) = 2*(q2*q3 + q0*q1);
    dcm(2,2) = q0*q0 - q1*q1 - q2*q2 + q3*q3;
    return dcm;
}
