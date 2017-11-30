/*
 * Academic License - for use in teaching, academic research, and meeting
 * course requirements at degree granting institutions only.  Not for
 * government, commercial, or other organizational use.
 *
 * File: Heater.c
 *
 * Code generated for Simulink model 'Heater'.
 *
 * Model version                  : 1.83
 * Simulink Coder version         : 8.11 (R2016b) 25-Aug-2016
 * C/C++ source code generated on : Tue Nov 28 10:16:24 2017
 *
 * Target selection: ert.tlc
 * Embedded hardware selection: Intel->x86-64 (Windows64)
 * Code generation objectives: Unspecified
 * Validation result: Not run
 */

#include "Heater.h"
#include "Heater_private.h"

/* Block signals (auto storage) */
B_Heater_T Heater_B;

/* Continuous states */
X_Heater_T Heater_X;

/* External inputs (root inport signals with auto storage) */
ExtU_Heater_T Heater_U;

/* External outputs (root outports fed by signals with auto storage) */
ExtY_Heater_T Heater_Y;

/* Real-time model */
RT_MODEL_Heater_T Heater_M_;
RT_MODEL_Heater_T *const Heater_M = &Heater_M_;

/*
 * This function updates continuous states using the ODE3 fixed-step
 * solver algorithm
 */
static void rt_ertODEUpdateContinuousStates(RTWSolverInfo *si )
{
  /* Solver Matrices */
  static const real_T rt_ODE3_A[3] = {
    1.0/2.0, 3.0/4.0, 1.0
  };

  static const real_T rt_ODE3_B[3][3] = {
    { 1.0/2.0, 0.0, 0.0 },

    { 0.0, 3.0/4.0, 0.0 },

    { 2.0/9.0, 1.0/3.0, 4.0/9.0 }
  };

  time_T t = rtsiGetT(si);
  time_T tnew = rtsiGetSolverStopTime(si);
  time_T h = rtsiGetStepSize(si);
  real_T *x = rtsiGetContStates(si);
  ODE3_IntgData *id = (ODE3_IntgData *)rtsiGetSolverData(si);
  real_T *y = id->y;
  real_T *f0 = id->f[0];
  real_T *f1 = id->f[1];
  real_T *f2 = id->f[2];
  real_T hB[3];
  int_T i;
  int_T nXc = 4;
  rtsiSetSimTimeStep(si,MINOR_TIME_STEP);

  /* Save the state values at time t in y, we'll use x as ynew. */
  (void) memcpy(y, x,
                (uint_T)nXc*sizeof(real_T));

  /* Assumes that rtsiSetT and ModelOutputs are up-to-date */
  /* f0 = f(t,y) */
  rtsiSetdX(si, f0);
  Heater_derivatives();

  /* f(:,2) = feval(odefile, t + hA(1), y + f*hB(:,1), args(:)(*)); */
  hB[0] = h * rt_ODE3_B[0][0];
  for (i = 0; i < nXc; i++) {
    x[i] = y[i] + (f0[i]*hB[0]);
  }

  rtsiSetT(si, t + h*rt_ODE3_A[0]);
  rtsiSetdX(si, f1);
  Heater_step();
  Heater_derivatives();

  /* f(:,3) = feval(odefile, t + hA(2), y + f*hB(:,2), args(:)(*)); */
  for (i = 0; i <= 1; i++) {
    hB[i] = h * rt_ODE3_B[1][i];
  }

  for (i = 0; i < nXc; i++) {
    x[i] = y[i] + (f0[i]*hB[0] + f1[i]*hB[1]);
  }

  rtsiSetT(si, t + h*rt_ODE3_A[1]);
  rtsiSetdX(si, f2);
  Heater_step();
  Heater_derivatives();

  /* tnew = t + hA(3);
     ynew = y + f*hB(:,3); */
  for (i = 0; i <= 2; i++) {
    hB[i] = h * rt_ODE3_B[2][i];
  }

  for (i = 0; i < nXc; i++) {
    x[i] = y[i] + (f0[i]*hB[0] + f1[i]*hB[1] + f2[i]*hB[2]);
  }

  rtsiSetT(si, tnew);
  rtsiSetSimTimeStep(si,MAJOR_TIME_STEP);
}

/* Model step function */
void Heater_step(void)
{
  real_T rtb_Sum_a;
  real_T rtb_Saturate;
  if (rtmIsMajorTimeStep(Heater_M)) {
    /* set solver stop time */
    rtsiSetSolverStopTime(&Heater_M->solverInfo,((Heater_M->Timing.clockTick0+1)*
      Heater_M->Timing.stepSize0));
  }                                    /* end MajorTimeStep */

  /* Update absolute time of base rate at minor time step */
  if (rtmIsMinorTimeStep(Heater_M)) {
    Heater_M->Timing.t[0] = rtsiGetT(&Heater_M->solverInfo);
  }

  /* Outport: '<Root>/r' incorporates:
   *  Integrator: '<S1>/r Integrator'
   */
  Heater_Y.r = Heater_X.rIntegrator_CSTATE;

  /* Outport: '<Root>/h' incorporates:
   *  Integrator: '<S1>/h Integrator'
   */
  Heater_Y.h = Heater_X.hIntegrator_CSTATE;

  /* Sum: '<S2>/Sum' incorporates:
   *  Inport: '<Root>/g'
   *  Integrator: '<S1>/r Integrator'
   */
  rtb_Sum_a = Heater_U.goaltemperature - Heater_X.rIntegrator_CSTATE;

  /* Gain: '<S3>/Filter Coefficient' incorporates:
   *  Gain: '<S3>/Derivative Gain'
   *  Integrator: '<S3>/Filter'
   *  Sum: '<S3>/SumD'
   */
  Heater_B.FilterCoefficient = (0.005 * rtb_Sum_a - Heater_X.Filter_CSTATE) *
    100.0;

  /* Sum: '<S3>/Sum' incorporates:
   *  Gain: '<S3>/Proportional Gain'
   *  Integrator: '<S3>/Integrator'
   */
  rtb_Saturate = (0.005 * rtb_Sum_a + Heater_X.Integrator_CSTATE) +
    Heater_B.FilterCoefficient;

  /* Saturate: '<S3>/Saturate' */
  if (rtb_Saturate > 1.0) {
    rtb_Saturate = 1.0;
  } else {
    if (rtb_Saturate < 0.0) {
      rtb_Saturate = 0.0;
    }
  }

  /* End of Saturate: '<S3>/Saturate' */

  /* Outport: '<Root>/l' */
  Heater_Y.l = rtb_Saturate;

  /* Product: '<S1>/Divide' incorporates:
   *  Constant: '<S1>/heaterCoeff'
   *  Integrator: '<S1>/h Integrator'
   *  Integrator: '<S1>/r Integrator'
   *  Product: '<S1>/(HC + OT)*r'
   *  Product: '<S1>/HC*h'
   *  Sum: '<S1>/Sum3'
   */
  Heater_B.Divide = ((0.1 * Heater_X.hIntegrator_CSTATE + Heater_ConstB.OCOT) -
                     Heater_ConstB.Sum2 * Heater_X.rIntegrator_CSTATE) /
    Heater_ConstB.Sum4;

  /* Product: '<S1>/Divide1' incorporates:
   *  Constant: '<S1>/boilerTemp'
   *  Constant: '<S1>/heaterCoeff'
   *  Integrator: '<S1>/h Integrator'
   *  Integrator: '<S1>/r Integrator'
   *  Product: '<S1>/(HC + l)*h'
   *  Product: '<S1>/BT*l'
   *  Product: '<S1>/HC*r'
   *  Sum: '<S1>/Sum'
   *  Sum: '<S1>/Sum1'
   */
  Heater_B.Divide1 = ((Heater_X.rIntegrator_CSTATE * 0.1 - (0.1 + rtb_Saturate) *
                       Heater_X.hIntegrator_CSTATE) + rtb_Saturate * 90.0) /
    Heater_ConstB.Sum5;

  /* Gain: '<S3>/Integral Gain' */
  Heater_B.IntegralGain = 0.000114468896 * rtb_Sum_a;
  if (rtmIsMajorTimeStep(Heater_M)) {
    rt_ertODEUpdateContinuousStates(&Heater_M->solverInfo);

    /* Update absolute time for base rate */
    /* The "clockTick0" counts the number of times the code of this task has
     * been executed. The absolute time is the multiplication of "clockTick0"
     * and "Timing.stepSize0". Size of "clockTick0" ensures timer will not
     * overflow during the application lifespan selected.
     */
    ++Heater_M->Timing.clockTick0;
    Heater_M->Timing.t[0] = rtsiGetSolverStopTime(&Heater_M->solverInfo);

    {
      /* Update absolute timer for sample time: [0.001s, 0.0s] */
      /* The "clockTick1" counts the number of times the code of this task has
       * been executed. The resolution of this integer timer is 0.001, which is the step size
       * of the task. Size of "clockTick1" ensures timer will not overflow during the
       * application lifespan selected.
       */
      Heater_M->Timing.clockTick1++;
    }
  }                                    /* end MajorTimeStep */
}

/* Derivatives for root system: '<Root>' */
void Heater_derivatives(void)
{
  XDot_Heater_T *_rtXdot;
  _rtXdot = ((XDot_Heater_T *) Heater_M->derivs);

  /* Derivatives for Integrator: '<S1>/r Integrator' */
  _rtXdot->rIntegrator_CSTATE = Heater_B.Divide;

  /* Derivatives for Integrator: '<S1>/h Integrator' */
  _rtXdot->hIntegrator_CSTATE = Heater_B.Divide1;

  /* Derivatives for Integrator: '<S3>/Integrator' */
  _rtXdot->Integrator_CSTATE = Heater_B.IntegralGain;

  /* Derivatives for Integrator: '<S3>/Filter' */
  _rtXdot->Filter_CSTATE = Heater_B.FilterCoefficient;
}

/* Model initialize function */
void Heater_initialize(void)
{
  /* Registration code */

  /* initialize real-time model */
  (void) memset((void *)Heater_M, 0,
                sizeof(RT_MODEL_Heater_T));

  {
    /* Setup solver object */
    rtsiSetSimTimeStepPtr(&Heater_M->solverInfo, &Heater_M->Timing.simTimeStep);
    rtsiSetTPtr(&Heater_M->solverInfo, &rtmGetTPtr(Heater_M));
    rtsiSetStepSizePtr(&Heater_M->solverInfo, &Heater_M->Timing.stepSize0);
    rtsiSetdXPtr(&Heater_M->solverInfo, &Heater_M->derivs);
    rtsiSetContStatesPtr(&Heater_M->solverInfo, (real_T **)
                         &Heater_M->contStates);
    rtsiSetNumContStatesPtr(&Heater_M->solverInfo,
      &Heater_M->Sizes.numContStates);
    rtsiSetNumPeriodicContStatesPtr(&Heater_M->solverInfo,
      &Heater_M->Sizes.numPeriodicContStates);
    rtsiSetPeriodicContStateIndicesPtr(&Heater_M->solverInfo,
      &Heater_M->periodicContStateIndices);
    rtsiSetPeriodicContStateRangesPtr(&Heater_M->solverInfo,
      &Heater_M->periodicContStateRanges);
    rtsiSetErrorStatusPtr(&Heater_M->solverInfo, (&rtmGetErrorStatus(Heater_M)));
    rtsiSetRTModelPtr(&Heater_M->solverInfo, Heater_M);
  }

  rtsiSetSimTimeStep(&Heater_M->solverInfo, MAJOR_TIME_STEP);
  Heater_M->intgData.y = Heater_M->odeY;
  Heater_M->intgData.f[0] = Heater_M->odeF[0];
  Heater_M->intgData.f[1] = Heater_M->odeF[1];
  Heater_M->intgData.f[2] = Heater_M->odeF[2];
  Heater_M->contStates = ((X_Heater_T *) &Heater_X);
  rtsiSetSolverData(&Heater_M->solverInfo, (void *)&Heater_M->intgData);
  rtsiSetSolverName(&Heater_M->solverInfo,"ode3");
  rtmSetTPtr(Heater_M, &Heater_M->Timing.tArray[0]);
  Heater_M->Timing.stepSize0 = 0.001;

  /* block I/O */
  (void) memset(((void *) &Heater_B), 0,
                sizeof(B_Heater_T));

  /* states (continuous) */
  {
    (void) memset((void *)&Heater_X, 0,
                  sizeof(X_Heater_T));
  }

  /* external inputs */
  Heater_U.goaltemperature = 0.0;

  /* external outputs */
  (void) memset((void *)&Heater_Y, 0,
                sizeof(ExtY_Heater_T));

  /* InitializeConditions for Integrator: '<S1>/r Integrator' */
  Heater_X.rIntegrator_CSTATE = -5.0;

  /* InitializeConditions for Integrator: '<S1>/h Integrator' */
  Heater_X.hIntegrator_CSTATE = 20.0;

  /* InitializeConditions for Integrator: '<S3>/Integrator' */
  Heater_X.Integrator_CSTATE = 0.0;

  /* InitializeConditions for Integrator: '<S3>/Filter' */
  Heater_X.Filter_CSTATE = 0.0;
}

/* Model terminate function */
void Heater_terminate(void)
{
  /* (no terminate code required) */
}

/*
 * File trailer for generated code.
 *
 * [EOF]
 */
