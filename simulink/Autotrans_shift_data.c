/*
 * Academic License - for use in teaching, academic research, and meeting
 * course requirements at degree granting institutions only.  Not for
 * government, commercial, or other organizational use.
 *
 * File: Autotrans_shift_data.c
 *
 * Code generated for Simulink model 'Autotrans_shift'.
 *
 * Model version                  : 1.276
 * Simulink Coder version         : 8.11 (R2016b) 25-Aug-2016
 * C/C++ source code generated on : Mon Nov 20 15:56:25 2017
 *
 * Target selection: ert.tlc
 * Embedded hardware selection: 32-bit Generic
 * Code generation objectives: Unspecified
 * Validation result: Not run
 */

#include "Autotrans_shift.h"
#include "Autotrans_shift_private.h"

/* Constant parameters (auto storage) */
const ConstP_Autotrans_shift_T Autotrans_shift_ConstP = {
  /* Expression: downth
   * Referenced by: '<S3>/interp_down'
   */
  { 0.0, 5.0, 40.0, 50.0, 90.0, 100.0 },

  /* Pooled Parameter (Mixed Expressions)
   * Referenced by:
   *   '<S3>/interp_down'
   *   '<S3>/interp_up'
   *   '<S7>/Look-Up Table'
   */
  { 1.0, 2.0, 3.0, 4.0 },

  /* Expression: downtab
   * Referenced by: '<S3>/interp_down'
   */
  { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 5.0, 5.0, 5.0, 5.0, 30.0, 30.0, 20.0, 20.0,
    25.0, 30.0, 50.0, 50.0, 35.0, 35.0, 40.0, 50.0, 80.0, 80.0 },

  /* Expression: upth
   * Referenced by: '<S3>/interp_up'
   */
  { 0.0, 25.0, 35.0, 50.0, 90.0, 100.0 },

  /* Expression: uptab
   * Referenced by: '<S3>/interp_up'
   */
  { 10.0, 10.0, 15.0, 23.0, 40.0, 40.0, 30.0, 30.0, 30.0, 41.0, 70.0, 70.0, 50.0,
    50.0, 50.0, 60.0, 100.0, 100.0, 1.0E+6, 1.0E+6, 1.0E+6, 1.0E+6, 1.0E+6,
    1.0E+6 },

  /* Expression: thvec
   * Referenced by: '<S1>/EngineTorque'
   */
  { 0.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0 },

  /* Expression: nevec
   * Referenced by: '<S1>/EngineTorque'
   */
  { 799.99999999999989, 1200.0, 1599.9999999999998, 1999.9999999999998, 2400.0,
    2800.0000000000005, 3199.9999999999995, 3599.9999999999995,
    3999.9999999999995, 4400.0, 4800.0 },

  /* Expression: emap
   * Referenced by: '<S1>/EngineTorque'
   */
  { -40.0, 215.0, 245.0, 264.0, 264.0, 267.0, 267.0, 267.0, 267.0, 267.0, -44.0,
    117.0, 208.0, 260.0, 279.0, 290.0, 297.0, 301.0, 301.0, 301.0, -49.0, 85.0,
    178.0, 241.0, 282.0, 293.0, 305.0, 308.0, 312.0, 312.0, -53.0, 66.0, 148.0,
    219.0, 275.0, 297.0, 305.0, 312.0, 319.0, 319.0, -57.0, 44.0, 122.0, 193.0,
    260.0, 290.0, 305.0, 319.0, 327.0, 327.0, -61.0, 29.0, 104.0, 167.0, 238.0,
    275.0, 301.0, 323.0, 327.0, 334.0, -65.0, 10.0, 85.0, 152.0, 223.0, 260.0,
    293.0, 319.0, 327.0, 334.0, -70.0, -2.0, 66.0, 133.0, 208.0, 256.0, 282.0,
    316.0, 327.0, 334.0, -74.0, -13.0, 48.0, 119.0, 189.0, 234.0, 267.0, 297.0,
    312.0, 319.0, -78.0, -22.0, 33.0, 96.0, 171.0, 212.0, 249.0, 279.0, 293.0,
    305.0, -82.0, -32.0, 18.0, 85.0, 152.0, 193.0, 226.0, 253.0, 267.0, 275.0 },

  /* Expression: [2.393 1.450 1.000 0.677]
   * Referenced by: '<S7>/Look-Up Table'
   */
  { 2.393, 1.45, 1.0, 0.677 },

  /* Pooled Parameter (Expression: speedratio)
   * Referenced by:
   *   '<S6>/FactorK'
   *   '<S6>/TorqueRatio'
   */
  { 0.0, 0.1, 0.2, 0.30000000000000004, 0.4, 0.5, 0.60000000000000009,
    0.70000000000000007, 0.8, 0.81, 0.82000000000000006, 0.83000000000000007,
    0.84, 0.85, 0.86, 0.87, 0.88, 0.89, 0.9, 0.92, 0.94 },

  /* Expression: Kfactor
   * Referenced by: '<S6>/FactorK'
   */
  { 137.4652089938063, 137.06501915685197, 135.86444964598905,
    135.66435472751189, 137.56525645304487, 140.36658531172509,
    145.26891081441539, 152.87251771654735, 162.97731109964374,
    164.2779280697452, 166.17882979527823, 167.97968406157264,
    170.08068070558275, 172.78196210502438, 175.38319604522741,
    179.58518933324765, 183.58708770279083, 189.89007763482121,
    197.69377945543027, 215.90241703685155, 244.51599037908485 },

  /* Expression: Torkratio
   * Referenced by: '<S6>/TorqueRatio'
   */
  { 2.2319999999999998, 2.075, 1.975, 1.8459999999999999, 1.72, 1.564, 1.409,
    1.254, 1.0959999999999999, 1.08, 1.061, 1.043, 1.028, 1.012, 1.002, 1.002,
    1.001, 0.998, 0.99900000000000011, 1.001, 1.002 }
};

/*
 * File trailer for generated code.
 *
 * [EOF]
 */
