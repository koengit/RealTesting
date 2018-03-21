/*
 * Academic License - for use in teaching, academic research, and meeting
 * course requirements at degree granting institutions only.  Not for
 * government, commercial, or other organizational use.
 *
 * File: AbstractFuelControl_M1_data.c
 *
 * Code generated for Simulink model 'AbstractFuelControl_M1'.
 *
 * Model version                  : 1.425
 * Simulink Coder version         : 8.11 (R2016b) 25-Aug-2016
 * C/C++ source code generated on : Fri Nov 24 15:32:49 2017
 *
 * Target selection: ert.tlc
 * Embedded hardware selection: 32-bit Generic
 * Emulation hardware selection:
 *    Differs from embedded hardware (MATLAB Host)
 * Code generation objectives: Unspecified
 * Validation result: Not run
 */

#include "AbstractFuelControl_M1.h"
#include "AbstractFuelControl_M1_private.h"

/* Invariant block signals (auto storage) */
const ConstB_AbstractFuelControl_M1_T AbstractFuelControl_M1_ConstB = {
  70.0F                                /* '<S15>/Sum' */
};

/* Constant parameters (auto storage) */
const ConstP_AbstractFuelControl_M1_T AbstractFuelControl_M1_ConstP = {
  /* Expression: reshape([0.8,.7,.7,.8,.9,.7,.66,.65,.73,.85,.66,.66,.63,.66,.8,.6,.6,.6,.6,.7],5,4)
   * Referenced by: '<S6>/1-Kappa'
   */
  { 0.8, 0.7, 0.7, 0.8, 0.9, 0.7, 0.66, 0.65, 0.73, 0.85, 0.66, 0.66, 0.63, 0.66,
    0.8, 0.6, 0.6, 0.6, 0.6, 0.7 },

  /* Pooled Parameter (Expression: [1000,1500,2000,2500,3000])
   * Referenced by:
   *   '<S6>/1-Kappa'
   *   '<S6>/tau_ww'
   */
  { 1000.0, 1500.0, 2000.0, 2500.0, 3000.0 },

  /* Pooled Parameter (Mixed Expressions)
   * Referenced by:
   *   '<S6>/1-Kappa'
   *   '<S6>/tau_ww'
   */
  { 0.1, 0.2, 0.3, 0.4 },

  /* Expression: reshape([.4,.3,.35,.3,.2,.22,.22,.4,.35,.5,.20,.22,.5,.4,.35,.35,.3,.45,.5,.4],5,4)
   * Referenced by: '<S6>/tau_ww'
   */
  { 0.4, 0.3, 0.35, 0.3, 0.2, 0.22, 0.22, 0.4, 0.35, 0.5, 0.2, 0.22, 0.5, 0.4,
    0.35, 0.35, 0.3, 0.45, 0.5, 0.4 },

  /* Expression: reshape([0.8,0.6,0.4,0.3,0.2,0.4,0.3,0.2,0.2,0.2,0.3,0.25,0.2,0.2,0.2,0.25,0.2,0.2,0.2,0.2],5,4)
   * Referenced by: '<S3>/delay (s)'
   */
  { 0.8, 0.6, 0.4, 0.3, 0.2, 0.4, 0.3, 0.2, 0.2, 0.2, 0.3, 0.25, 0.2, 0.2, 0.2,
    0.25, 0.2, 0.2, 0.2, 0.2 },

  /* Expression: [800,1000,1500,2000,3000]
   * Referenced by: '<S3>/delay (s)'
   */
  { 800.0, 1000.0, 1500.0, 2000.0, 3000.0 },

  /* Expression: [0.05,0.15,0.2,0.25]
   * Referenced by: '<S3>/delay (s)'
   */
  { 0.05, 0.15, 0.2, 0.25 },

  /* Pooled Parameter (Expression: )
   * Referenced by:
   *   '<S3>/delay (s)'
   *   '<S6>/1-Kappa'
   *   '<S6>/tau_ww'
   */
  { 4U, 3U }
};

/*
 * File trailer for generated code.
 *
 * [EOF]
 */
