# Yaw pitch roll

Your equations are correct only if the order of rotations is: roll, then pitch, then yaw. For the record, the
correspondence with Euler angles (with respect to the frame of reference implicitly given with the transformation
matrix) is as follows:

- Roll is the rotation about the x axis (between -180 and 180 deg);
- Pitch is the rotations about the y axis (between -90 and 90 deg);
- Yaw is the rotation about the z axis (between -180 and 180).

Given these, the order roll, pitch, yaw mentioned in the first sentence corresponds to the rotation matrix obtain by the
matrix product Rz Ry Rx (in this order). Note that your formula give the values of these angles in radians (multiply by
180 and divide by pi to obtain values in degrees). All rotations are counter-clockwise with respect to the axis.

![img](https://i.stack.imgur.com/65EKz.png)
