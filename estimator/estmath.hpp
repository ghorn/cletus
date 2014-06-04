#pragma once

Eigen::Vector4d quat_mult(const Eigen::Vector4d &q, const Eigen::Vector4d &p);
Eigen::Matrix3d dcm_of_quat(const Eigen::Vector4d &q);
