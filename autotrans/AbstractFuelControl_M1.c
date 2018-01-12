/*
 * Academic License - for use in teaching, academic research, and meeting
 * course requirements at degree granting institutions only.  Not for
 * government, commercial, or other organizational use.
 *
 * File: AbstractFuelControl_M1.c
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

/* Block signals (auto storage) */
B_AbstractFuelControl_M1_T AbstractFuelControl_M1_B;

/* Continuous states */
X_AbstractFuelControl_M1_T AbstractFuelControl_M1_X;

/* Block states (auto storage) */
DW_AbstractFuelControl_M1_T AbstractFuelControl_M1_DW;

/* Previous zero-crossings (trigger) states */
PrevZCX_AbstractFuelControl_M_T AbstractFuelControl_M1_PrevZCX;

/* External inputs (root inport signals with auto storage) */
ExtU_AbstractFuelControl_M1_T AbstractFuelControl_M1_U;

/* External outputs (root outports fed by signals with auto storage) */
ExtY_AbstractFuelControl_M1_T AbstractFuelControl_M1_Y;

/* Real-time model */
RT_MODEL_AbstractFuelControl__T AbstractFuelControl_M1_M_;
RT_MODEL_AbstractFuelControl__T *const AbstractFuelControl_M1_M =
  &AbstractFuelControl_M1_M_;
static void rate_scheduler(void);

/* For variable transport delay block, find the real delay time */
real_T rt_VTDelayfindtDInterpolate(
  real_T x,real_T* tBuf,real_T* uBuf,real_T* xBuf,int_T bufSz,int_T head,int_T
  tail,int_T* pLast,real_T t,real_T tStart,boolean_T discrete,boolean_T
  minorStepAndTAtLastMajorOutput,real_T initOutput,real_T* appliedDelay)
{
  int_T n, k;
  real_T f;
  int_T kp1;
  real_T tminustD, tL, tR, uD, uL, uR, fU;
  if (minorStepAndTAtLastMajorOutput) {
    /* pretend that the entry at head has not been added */
    if (*pLast == head) {
      *pLast = (*pLast == 0) ? bufSz-1 : *pLast-1;
    }

    head = (head == 0) ? bufSz-1 : head-1;
  }

  /*
   * The loop below finds k such that:
   *      x(t)-x(tminustD) =1 or
   *      x - xBuf[k+1] <= 1.0 < x - xBuf[k]
   *
   * Note that we have:
   *
   * tStart = tBuf[0] < tBuf[1] < ... < tBuf[tail] < ... tBuf[head] <= t
   *      0 = xBuf[0] < xBuf[1] < ... < xBuf[tail] < ... xBuf[head] <  x
   *
   * This is true if we assume the direction of transport is always positive
   * such as a flow goes through a pipe from one end to another. However, for
   * model such as convey belt, the transportation can change direction. For
   * this case, there will be more than one solution to x(t)-x(tminustD) = 1,
   * should found the minimum tminustD and tminustD > 0. The search will not
   * be as efficient as the following code.
   */

  /*
   * when x<=1, physically it means the flow didn't reach the output yet,
   * t-tD will be less then zero, so force output to be the initial output
   */
  if (x <= 1) {
    return initOutput;
  }

  /*
   * if the x is monoton increase, only one solution. use k=pLast for now
   */
  k= *pLast;
  n = 0;
  for (;;) {
    n++;
    if (n>bufSz)
      break;
    if (x - xBuf[k] > 1.0) {
      /* move k forward, unless k = head */
      if (k == head) {
        /* xxx this situation means tD= appliedDelay = 0
         *
         * linearly interpolate using (tBuf[head], xBuf[head])
         * and (t,x) to find (tD,xD) such that: x - xD = 1.0
         */
        int_T km1;
        f = (x - 1.0 - xBuf[k]) / (x - xBuf[k]);
        tminustD = (1.0-f)*tBuf[k] + f*t;
        km1 = k-1;
        if (km1 < 0)
          km1 = bufSz-1;
        tL = tBuf[km1];
        tR = tBuf[k];
        uL = uBuf[km1];
        uR = uBuf[k];
        break;
      }

      kp1 = k+1;
      if (kp1 == bufSz)
        kp1 = 0;
      if (x - xBuf[kp1] <= 1.0) {
        /*
         * linearly interpolate using (tBuf[k], xBuf[k])
         * and  (tBuf[k+1], xBuf[k+1]) to find (tminustD,xD)
         * such that: x - xD = 1.0
         */
        f = (x - 1.0 - xBuf[k]) / (xBuf[kp1] - xBuf[k]);
        tL = tBuf[k];
        tR = tBuf[kp1];
        uL = uBuf[k];
        uR = uBuf[kp1];
        tminustD = (1.0-f)*tL + f*tR;
        break;
      }

      k = kp1;
    } else {
      /* moved k backward, unless k = tail */
      if (k == tail) {
        /* This situation means tminustD <= Ttail*/
        f = (x - 1.0)/xBuf[k];
        if (discrete) {
          return(uBuf[tail]);
        }

        kp1 = k+1;
        if (kp1 == bufSz)
          kp1 = 0;

        /* * linearly interpolate using (tStart, 0)
         * and  (tBuf[tail], xBuf[tail]) to find (tminustD,xD)
         * such that: x - xD = 1.0
         */

        /* Here it is better to use Tstart because since x>1, tminustD
         * must > 0. Since x is monotone increase, its linearity is
         * better.
         */
        tminustD = (1-f)*tStart + f*tBuf[k];

        /* linearly interpolate using (t[tail], x[tail])
         * and  (tBuf[tail+1], xBuf[tail+1]) to find (tminustD,xD)
         * such that: x - xD = 1.0.
         * For time delay block, use t[tail] and t[tail+1], not good
         * for transport delay block since it may give tminstD < 0
         */

        /*  f = (tBuf[kp1]-tBuf[k])/(xBuf[kp1]-xBuf[k]);
         *  tminustD = tBuf[kp1]-f*(1+xBuf[kp1]-x);
         */
        tL = tBuf[k];
        tR = tBuf[kp1];
        uL = uBuf[k];
        uR = uBuf[kp1];
        break;
      }

      k = k - 1;
      if (k < 0)
        k = bufSz-1;
    }
  }

  *pLast = k;
  if (tR == tL) {
    fU = 1.0;
  } else {
    fU = (tminustD-tL)/(tR-tL);
  }

  /* for discrete signal, no interpolation, use either uL or uR
   * depend on wehre tminustD is.
   */
  if (discrete) {
    uD= (fU > (1.0-fU))? uR: uL;
  } else {
    uD = (1.0-fU)*uL + fU*uR;
  }

  /* we want return tD= t-(t-tD);*/
  *appliedDelay = t-tminustD;
  return uD;
}

real_T look2_binlxpw(real_T u0, real_T u1, const real_T bp0[], const real_T bp1[],
                     const real_T table[], const uint32_T maxIndex[], uint32_T
                     stride)
{
  real_T frac;
  uint32_T bpIndices[2];
  real_T fractions[2];
  real_T yL_1d;
  uint32_T iRght;
  uint32_T bpIdx;
  uint32_T iLeft;

  /* Lookup 2-D
     Search method: 'binary'
     Use previous index: 'off'
     Interpolation method: 'Linear'
     Extrapolation method: 'Linear'
     Use last breakpoint for index at or above upper limit: 'off'
     Remove protection against out-of-range input in generated code: 'off'
   */
  /* Prelookup - Index and Fraction
     Index Search method: 'binary'
     Extrapolation method: 'Linear'
     Use previous index: 'off'
     Use last breakpoint for index at or above upper limit: 'off'
     Remove protection against out-of-range input in generated code: 'off'
   */
  if (u0 <= bp0[0U]) {
    iLeft = 0U;
    frac = (u0 - bp0[0U]) / (bp0[1U] - bp0[0U]);
  } else if (u0 < bp0[maxIndex[0U]]) {
    /* Binary Search */
    bpIdx = maxIndex[0U] >> 1U;
    iLeft = 0U;
    iRght = maxIndex[0U];
    while (iRght - iLeft > 1U) {
      if (u0 < bp0[bpIdx]) {
        iRght = bpIdx;
      } else {
        iLeft = bpIdx;
      }

      bpIdx = (iRght + iLeft) >> 1U;
    }

    frac = (u0 - bp0[iLeft]) / (bp0[iLeft + 1U] - bp0[iLeft]);
  } else {
    iLeft = maxIndex[0U] - 1U;
    frac = (u0 - bp0[maxIndex[0U] - 1U]) / (bp0[maxIndex[0U]] - bp0[maxIndex[0U]
      - 1U]);
  }

  fractions[0U] = frac;
  bpIndices[0U] = iLeft;

  /* Prelookup - Index and Fraction
     Index Search method: 'binary'
     Extrapolation method: 'Linear'
     Use previous index: 'off'
     Use last breakpoint for index at or above upper limit: 'off'
     Remove protection against out-of-range input in generated code: 'off'
   */
  if (u1 <= bp1[0U]) {
    iLeft = 0U;
    frac = (u1 - bp1[0U]) / (bp1[1U] - bp1[0U]);
  } else if (u1 < bp1[maxIndex[1U]]) {
    /* Binary Search */
    bpIdx = maxIndex[1U] >> 1U;
    iLeft = 0U;
    iRght = maxIndex[1U];
    while (iRght - iLeft > 1U) {
      if (u1 < bp1[bpIdx]) {
        iRght = bpIdx;
      } else {
        iLeft = bpIdx;
      }

      bpIdx = (iRght + iLeft) >> 1U;
    }

    frac = (u1 - bp1[iLeft]) / (bp1[iLeft + 1U] - bp1[iLeft]);
  } else {
    iLeft = maxIndex[1U] - 1U;
    frac = (u1 - bp1[maxIndex[1U] - 1U]) / (bp1[maxIndex[1U]] - bp1[maxIndex[1U]
      - 1U]);
  }

  /* Interpolation 2-D
     Interpolation method: 'Linear'
     Use last breakpoint for index at or above upper limit: 'off'
     Overflow mode: 'portable wrapping'
   */
  bpIdx = iLeft * stride + bpIndices[0U];
  yL_1d = (table[bpIdx + 1U] - table[bpIdx]) * fractions[0U] + table[bpIdx];
  bpIdx += stride;
  return (((table[bpIdx + 1U] - table[bpIdx]) * fractions[0U] + table[bpIdx]) -
          yL_1d) * frac + yL_1d;
}

/*
 *   This function updates active task flag for each subrate.
 * The function is called at model base rate, hence the
 * generated code self-manages all its subrates.
 */
static void rate_scheduler(void)
{
  /* Compute which subrates run during the next base time step.  Subrates
   * are an integer multiple of the base rate counter.  Therefore, the subtask
   * counter is reset when it reaches its limit (zero means run).
   */
  (AbstractFuelControl_M1_M->Timing.TaskCounters.TID[2])++;
  if ((AbstractFuelControl_M1_M->Timing.TaskCounters.TID[2]) > 1) {/* Sample time: [0.01s, 0.0s] */
    AbstractFuelControl_M1_M->Timing.TaskCounters.TID[2] = 0;
  }
}

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
  int_T nXc = 6;
  rtsiSetSimTimeStep(si,MINOR_TIME_STEP);

  /* Save the state values at time t in y, we'll use x as ynew. */
  (void) memcpy(y, x,
                (uint_T)nXc*sizeof(real_T));

  /* Assumes that rtsiSetT and ModelOutputs are up-to-date */
  /* f0 = f(t,y) */
  rtsiSetdX(si, f0);
  AbstractFuelControl_M1_derivatives();

  /* f(:,2) = feval(odefile, t + hA(1), y + f*hB(:,1), args(:)(*)); */
  hB[0] = h * rt_ODE3_B[0][0];
  for (i = 0; i < nXc; i++) {
    x[i] = y[i] + (f0[i]*hB[0]);
  }

  rtsiSetT(si, t + h*rt_ODE3_A[0]);
  rtsiSetdX(si, f1);
  AbstractFuelControl_M1_step();
  AbstractFuelControl_M1_derivatives();

  /* f(:,3) = feval(odefile, t + hA(2), y + f*hB(:,2), args(:)(*)); */
  for (i = 0; i <= 1; i++) {
    hB[i] = h * rt_ODE3_B[1][i];
  }

  for (i = 0; i < nXc; i++) {
    x[i] = y[i] + (f0[i]*hB[0] + f1[i]*hB[1]);
  }

  rtsiSetT(si, t + h*rt_ODE3_A[1]);
  rtsiSetdX(si, f2);
  AbstractFuelControl_M1_step();
  AbstractFuelControl_M1_derivatives();

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
void AbstractFuelControl_M1_step(void)
{
  /* local block i/o variables */
  real_T rtb_fuelsystemtransportdelay;
  real32_T rtb_DataStoreRead2;
  real_T rtb_AF_sensor;
  boolean_T rtb_DataStoreRead1_l;
  real_T rtb_radstorpm;
  real_T rtb_ftheta;
  real_T rtb_Integrator;
  real_T rtb_Kappatolerance0911;
  real32_T rtb_DataStoreRead;
  real_T rtb_fuel_puddle_evap;
  real_T rtb_pratio;
  real_T rtb_PulseGenerator_10ms;
  ZCEventType zcEvent;
  boolean_T rtb_LogicalOperator_c;
  boolean_T rtb_LogicalOperator;
  boolean_T rtb_RelationalOperator1;
  real32_T rtb_Sum_o;
  real32_T rtb_Sum1_m;
  real32_T rtb_Sum2;
  if (rtmIsMajorTimeStep(AbstractFuelControl_M1_M)) {
    /* set solver stop time */
    rtsiSetSolverStopTime(&AbstractFuelControl_M1_M->solverInfo,
                          ((AbstractFuelControl_M1_M->Timing.clockTick0+1)*
      AbstractFuelControl_M1_M->Timing.stepSize0));
  }                                    /* end MajorTimeStep */

  /* Update absolute time of base rate at minor time step */
  if (rtmIsMinorTimeStep(AbstractFuelControl_M1_M)) {
    AbstractFuelControl_M1_M->Timing.t[0] = rtsiGetT
      (&AbstractFuelControl_M1_M->solverInfo);
  }

  /* Outputs for Atomic SubSystem: '<Root>/Model 1' */
  /* Saturate: '<S1>/Engine Speed [900 1100]' incorporates:
   *  Inport: '<Root>/Engine Speed'
   */
  if (AbstractFuelControl_M1_U.EngineSpeed > 3100.0) {
    rtb_ftheta = 3100.0;
  } else if (AbstractFuelControl_M1_U.EngineSpeed < 900.0) {
    rtb_ftheta = 900.0;
  } else {
    rtb_ftheta = AbstractFuelControl_M1_U.EngineSpeed;
  }

  /* End of Saturate: '<S1>/Engine Speed [900 1100]' */

  /* Gain: '<S1>/(rpm) to (rad//s)' */
  rtb_radstorpm = 0.10471975511965977 * rtb_ftheta;

  /* Gain: '<S17>/A//F_sensor' incorporates:
   *  Integrator: '<S19>/Integrator'
   */
  rtb_AF_sensor = 1.0 * AbstractFuelControl_M1_X.Integrator_CSTATE;

  /* TransferFcn: '<S1>/Throttle delay' */
  rtb_fuelsystemtransportdelay = 0.0;
  rtb_fuelsystemtransportdelay += 10.0 *
    AbstractFuelControl_M1_X.Throttledelay_CSTATE;

  /* Sum: '<S1>/Sum' incorporates:
   *  Constant: '<S1>/Base opening angle'
   */
  rtb_fuelsystemtransportdelay += 8.8;

  /* Saturate: '<S1>/theta [0 90]' */
  if (rtb_fuelsystemtransportdelay > 90.0) {
    rtb_fuelsystemtransportdelay = 90.0;
  } else {
    if (rtb_fuelsystemtransportdelay < 0.0) {
      rtb_fuelsystemtransportdelay = 0.0;
    }
  }

  /* End of Saturate: '<S1>/theta [0 90]' */

  /* Outputs for Atomic SubSystem: '<S1>/AF_Controller' */
  /* Outputs for Atomic SubSystem: '<S2>/fuel_controller' */
  /* DataStoreWrite: '<S7>/DataStoreWrite3' incorporates:
   *  DataTypeConversion: '<S1>/Data Type Conversion4'
   */
  AbstractFuelControl_M1_DW.throttle_angle = (real32_T)
    rtb_fuelsystemtransportdelay;

  /* End of Outputs for SubSystem: '<S2>/fuel_controller' */
  /* End of Outputs for SubSystem: '<S1>/AF_Controller' */

  /* Fcn: '<S5>/f(theta)' */
  rtb_ftheta = ((2.821 - 0.05231 * rtb_fuelsystemtransportdelay) + 0.10299 *
                rtb_fuelsystemtransportdelay * rtb_fuelsystemtransportdelay) -
    0.00063 * rtb_fuelsystemtransportdelay * rtb_fuelsystemtransportdelay *
    rtb_fuelsystemtransportdelay;

  /* Integrator: '<S4>/p0 = 0.543 (bar)' */
  rtb_fuelsystemtransportdelay = AbstractFuelControl_M1_X.p00543bar_CSTATE;

  /* Product: '<S5>/Product1' incorporates:
   *  Constant: '<S1>/Atmospheric Pressure (bar)'
   */
  rtb_Integrator = rtb_fuelsystemtransportdelay / 1.0;

  /* Product: '<S5>/Product2' incorporates:
   *  Constant: '<S1>/Atmospheric Pressure (bar)'
   */
  rtb_pratio = 1.0 / rtb_fuelsystemtransportdelay * 1.0;

  /* MinMax: '<S5>/MinMax' */
  if ((rtb_Integrator <= rtb_pratio) || rtIsNaN(rtb_pratio)) {
    rtb_pratio = rtb_Integrator;
  }

  /* End of MinMax: '<S5>/MinMax' */

  /* Switch: '<S5>/Switch' incorporates:
   *  Constant: '<S5>/Sonic Flow '
   *  Fcn: '<S5>/g(pratio)'
   */
  if (rtb_pratio >= 0.5) {
    /* Fcn: '<S5>/g(pratio)' */
    rtb_pratio -= rtb_pratio * rtb_pratio;
    if (rtb_pratio < 0.0) {
      rtb_pratio = -sqrt(-rtb_pratio);
    } else {
      rtb_pratio = sqrt(rtb_pratio);
    }

    rtb_pratio *= 2.0;
  } else {
    rtb_pratio = 1.0;
  }

  /* End of Switch: '<S5>/Switch' */

  /* Sum: '<S5>/Sum' incorporates:
   *  Constant: '<S1>/Atmospheric Pressure (bar)'
   */
  rtb_Integrator = 1.0 - rtb_fuelsystemtransportdelay;

  /* Signum: '<S5>/flow direction' */
  if (rtb_Integrator < 0.0) {
    rtb_Integrator = -1.0;
  } else if (rtb_Integrator > 0.0) {
    rtb_Integrator = 1.0;
  } else {
    if (rtb_Integrator == 0.0) {
      rtb_Integrator = 0.0;
    }
  }

  /* End of Signum: '<S5>/flow direction' */

  /* Product: '<S5>/Product' */
  rtb_pratio = rtb_ftheta * rtb_pratio * rtb_Integrator;

  /* Outputs for Atomic SubSystem: '<S1>/AF_Controller' */
  /* Step: '<S2>/Pwon' */
  if (AbstractFuelControl_M1_M->Timing.t[0] < 0.001) {
    AbstractFuelControl_M1_B.Pwon = 0.0;
  } else {
    AbstractFuelControl_M1_B.Pwon = 1.0;
  }

  /* End of Step: '<S2>/Pwon' */
  if (rtmIsMajorTimeStep(AbstractFuelControl_M1_M) &&
      AbstractFuelControl_M1_M->Timing.TaskCounters.TID[1] == 0) {
    /* DiscretePulseGenerator: '<S2>/PulseGenerator_10ms' */
    rtb_PulseGenerator_10ms = (AbstractFuelControl_M1_DW.clockTickCounter < 1.0)
      && (AbstractFuelControl_M1_DW.clockTickCounter >= 0) ? 1.0 : 0.0;
    if (AbstractFuelControl_M1_DW.clockTickCounter >= 2.0 - 1.0) {
      AbstractFuelControl_M1_DW.clockTickCounter = 0;
    } else {
      AbstractFuelControl_M1_DW.clockTickCounter++;
    }

    /* End of DiscretePulseGenerator: '<S2>/PulseGenerator_10ms' */
  }

  /* Outputs for Atomic SubSystem: '<S2>/fuel_controller' */
  /* DataStoreWrite: '<S7>/DataStoreWrite' incorporates:
   *  DataTypeConversion: '<S1>/Data Type Conversion1'
   */
  AbstractFuelControl_M1_DW.engine_speed = (real32_T)rtb_radstorpm;

  /* DataStoreWrite: '<S7>/DataStoreWrite1' incorporates:
   *  DataTypeConversion: '<S1>/Data Type Conversion2'
   *  Gain: '<S1>/MAF sensor tolerance [0.95 1.05]'
   */
  AbstractFuelControl_M1_DW.throttle_flow = (real32_T)(1.0 * rtb_pratio);

  /* End of Outputs for SubSystem: '<S2>/fuel_controller' */
  /* End of Outputs for SubSystem: '<S1>/AF_Controller' */

  /* Step: '<S1>/A//F Sensor Fault Injection' */
  if (AbstractFuelControl_M1_M->Timing.t[0] < 50.0) {
    rtb_ftheta = 0.0;
  } else {
    rtb_ftheta = 1.0;
  }

  /* End of Step: '<S1>/A//F Sensor Fault Injection' */

  /* Switch: '<S3>/Switch' incorporates:
   *  Constant: '<S3>/FaultySensorOutput'
   */
  if (rtb_ftheta >= 0.5) {
    rtb_ftheta = (-1.0);
  } else {
    rtb_ftheta = rtb_AF_sensor;
  }

  /* End of Switch: '<S3>/Switch' */

  /* Outputs for Atomic SubSystem: '<S1>/AF_Controller' */
  /* Outputs for Atomic SubSystem: '<S2>/fuel_controller' */
  /* DataStoreWrite: '<S7>/DataStoreWrite2' incorporates:
   *  DataTypeConversion: '<S1>/Data Type Conversion3'
   *  Gain: '<S1>/A//F sensor tolerance [0.99 1.01]'
   */
  AbstractFuelControl_M1_DW.airbyfuel_meas = (real32_T)(1.0 * rtb_ftheta);

  /* Outputs for Triggered SubSystem: '<S7>/fuel_controller_10ms' incorporates:
   *  TriggerPort: '<S8>/Function'
   */
  /* Outputs for Triggered SubSystem: '<S7>/fuel_controller_mode_10ms' incorporates:
   *  TriggerPort: '<S9>/Function'
   */
  /* Outputs for Triggered SubSystem: '<S7>/fuel_controller_pwon' incorporates:
   *  TriggerPort: '<S10>/Function'
   */
  if ((rtmIsMajorTimeStep(AbstractFuelControl_M1_M) &&
       AbstractFuelControl_M1_M->Timing.TaskCounters.TID[1] == 0) &&
      rtmIsMajorTimeStep(AbstractFuelControl_M1_M)) {
    zcEvent = rt_ZCFcn(RISING_ZERO_CROSSING,
                       &AbstractFuelControl_M1_PrevZCX.fuel_controller_pwon_Trig_ZCE,
                       (AbstractFuelControl_M1_B.Pwon));
    if (zcEvent != NO_ZCEVENT) {
      /* DataStoreWrite: '<S10>/DataStoreWrite1' incorporates:
       *  Constant: '<S10>/Constant1'
       *  DataTypeConversion: '<S10>/Data Type Conversion'
       */
      AbstractFuelControl_M1_DW.controller_mode = (1.0F != 0.0F);

      /* DataStoreWrite: '<S10>/DataStoreWrite' incorporates:
       *  Constant: '<S10>/Constant2'
       */
      AbstractFuelControl_M1_DW.commanded_fuel = 0.1726F;

      /* DataStoreWrite: '<S10>/DataStoreWrite2' incorporates:
       *  Constant: '<S10>/Constant3'
       */
      AbstractFuelControl_M1_DW.airbyfuel_ref = 14.7F;
    }

    zcEvent = rt_ZCFcn(RISING_ZERO_CROSSING,
                       &AbstractFuelControl_M1_PrevZCX.fuel_controller_mode_10ms_Trig_,
                       (rtb_PulseGenerator_10ms));
    if (zcEvent != NO_ZCEVENT) {
      /* Outputs for Atomic SubSystem: '<S9>/sensor_failure_detection' */
      /* Logic: '<S16>/Logical Operator' incorporates:
       *  Constant: '<S16>/threshold'
       *  DataStoreRead: '<S9>/DataStoreRead2'
       *  RelationalOperator: '<S16>/Relational Operator'
       *  UnitDelay: '<S16>/Unit Delay'
       */
      rtb_LogicalOperator = ((AbstractFuelControl_M1_DW.airbyfuel_meas <= (-1.0F))
        || AbstractFuelControl_M1_DW.UnitDelay_DSTATE);

      /* Update for UnitDelay: '<S16>/Unit Delay' */
      AbstractFuelControl_M1_DW.UnitDelay_DSTATE = rtb_LogicalOperator;

      /* End of Outputs for SubSystem: '<S9>/sensor_failure_detection' */

      /* Outputs for Atomic SubSystem: '<S9>/normal_mode_detection' */
      /* Sum: '<S14>/Sum' incorporates:
       *  Constant: '<S14>/sampling_sec'
       *  UnitDelay: '<S14>/Unit Delay2'
       */
      rtb_Sum_o = AbstractFuelControl_M1_DW.UnitDelay2_DSTATE + 0.01F;

      /* Logic: '<S14>/Logical Operator' incorporates:
       *  Constant: '<S14>/normal_mode_start_sec'
       *  RelationalOperator: '<S14>/Relational Operator'
       *  UnitDelay: '<S14>/Unit Delay1'
       */
      rtb_LogicalOperator_c = ((rtb_Sum_o >= 10.0F) ||
        AbstractFuelControl_M1_DW.UnitDelay1_DSTATE_e);

      /* Update for UnitDelay: '<S14>/Unit Delay2' */
      AbstractFuelControl_M1_DW.UnitDelay2_DSTATE = rtb_Sum_o;

      /* Update for UnitDelay: '<S14>/Unit Delay1' */
      AbstractFuelControl_M1_DW.UnitDelay1_DSTATE_e = rtb_LogicalOperator_c;

      /* End of Outputs for SubSystem: '<S9>/normal_mode_detection' */

      /* Outputs for Atomic SubSystem: '<S9>/power_mode_detection' */
      /* Switch: '<S15>/Switch' incorporates:
       *  Constant: '<S15>/Constant'
       *  UnitDelay: '<S15>/Unit Delay1'
       */
      if (AbstractFuelControl_M1_DW.UnitDelay1_DSTATE_a) {
        rtb_Sum_o = 50.0F;
      } else {
        rtb_Sum_o = AbstractFuelControl_M1_ConstB.Sum;
      }

      /* End of Switch: '<S15>/Switch' */

      /* RelationalOperator: '<S15>/Relational Operator1' incorporates:
       *  DataStoreRead: '<S9>/DataStoreRead4'
       */
      rtb_RelationalOperator1 = (AbstractFuelControl_M1_DW.throttle_angle >=
        rtb_Sum_o);

      /* Update for UnitDelay: '<S15>/Unit Delay1' */
      AbstractFuelControl_M1_DW.UnitDelay1_DSTATE_a = rtb_RelationalOperator1;

      /* End of Outputs for SubSystem: '<S9>/power_mode_detection' */

      /* DataStoreWrite: '<S9>/DataStoreWrite' incorporates:
       *  Logic: '<S9>/Logical Operator1'
       *  Logic: '<S9>/Logical Operator2'
       */
      AbstractFuelControl_M1_DW.controller_mode = (rtb_LogicalOperator ||
        (!rtb_LogicalOperator_c) || rtb_RelationalOperator1);

      /* Switch: '<S9>/Switch' incorporates:
       *  Logic: '<S9>/Logical Operator3'
       */
      if (rtb_LogicalOperator_c && rtb_RelationalOperator1) {
        /* DataStoreWrite: '<S9>/DataStoreWrite1' incorporates:
         *  Constant: '<S9>/airbyfuel_reference_power'
         */
        AbstractFuelControl_M1_DW.airbyfuel_ref = 12.5F;
      } else {
        /* DataStoreWrite: '<S9>/DataStoreWrite1' incorporates:
         *  Constant: '<S9>/airbyfuel_reference'
         */
        AbstractFuelControl_M1_DW.airbyfuel_ref = 14.7F;
      }

      /* End of Switch: '<S9>/Switch' */
    }

    zcEvent = rt_ZCFcn(RISING_ZERO_CROSSING,
                       &AbstractFuelControl_M1_PrevZCX.fuel_controller_10ms_Trig_ZCE,
                       (rtb_PulseGenerator_10ms));
    if (zcEvent != NO_ZCEVENT) {
      /* Outputs for Atomic SubSystem: '<S8>/air_estimation' */
      /* Sum: '<S11>/Sum3' incorporates:
       *  Constant: '<S11>/Constant2'
       *  Constant: '<S11>/Constant3'
       *  Constant: '<S11>/Constant4'
       *  Constant: '<S11>/Constant5'
       *  DataStoreRead: '<S8>/DataStoreRead1'
       *  Product: '<S11>/Prod2'
       *  Product: '<S11>/Prod3'
       *  Product: '<S11>/Prod4'
       *  UnitDelay: '<S11>/UnitDelay1'
       */
      rtb_Sum_o = ((AbstractFuelControl_M1_DW.UnitDelay1_DSTATE_d *
                    AbstractFuelControl_M1_DW.engine_speed * 0.08979F + (-0.366F))
                   + AbstractFuelControl_M1_DW.UnitDelay1_DSTATE_d *
                   AbstractFuelControl_M1_DW.UnitDelay1_DSTATE_d *
                   AbstractFuelControl_M1_DW.engine_speed * (-0.0337F)) +
        AbstractFuelControl_M1_DW.engine_speed *
        AbstractFuelControl_M1_DW.engine_speed *
        AbstractFuelControl_M1_DW.UnitDelay1_DSTATE_d * 0.0001F;

      /* Update for UnitDelay: '<S11>/UnitDelay1' incorporates:
       *  Constant: '<S11>/Constant1'
       *  DataStoreRead: '<S8>/DataStoreRead'
       *  Gain: '<S11>/Gain'
       *  Product: '<S11>/Prod1'
       *  Sum: '<S11>/Sum1'
       *  Sum: '<S11>/Sum2'
       *  UnitDelay: '<S11>/UnitDelay1'
       */
      AbstractFuelControl_M1_DW.UnitDelay1_DSTATE_d +=
        (AbstractFuelControl_M1_DW.throttle_flow - rtb_Sum_o) * 0.41328F * 0.01F;

      /* End of Outputs for SubSystem: '<S8>/air_estimation' */

      /* Outputs for Enabled SubSystem: '<S8>/feedback_PI_controller' incorporates:
       *  EnablePort: '<S12>/Enable'
       */
      /* Logic: '<S8>/Logical Operator2' incorporates:
       *  DataStoreRead: '<S8>/DataStoreRead3'
       */
      if (!AbstractFuelControl_M1_DW.controller_mode) {
        /* Sum: '<S12>/Sum1' incorporates:
         *  DataStoreRead: '<S8>/DataStoreRead2'
         *  DataStoreRead: '<S8>/DataStoreRead4'
         */
        rtb_Sum1_m = AbstractFuelControl_M1_DW.airbyfuel_meas -
          AbstractFuelControl_M1_DW.airbyfuel_ref;

        /* Sum: '<S12>/Sum2' incorporates:
         *  Constant: '<S12>/Constant1'
         *  Gain: '<S12>/Gain1'
         *  Product: '<S12>/Prod1'
         *  UnitDelay: '<S12>/UnitDelay1'
         */
        rtb_Sum2 = 0.14F * rtb_Sum1_m * 0.01F +
          AbstractFuelControl_M1_DW.UnitDelay1_DSTATE;

        /* Sum: '<S12>/Sum3' incorporates:
         *  Gain: '<S12>/Gain'
         */
        AbstractFuelControl_M1_B.Sum3 = 0.04F * rtb_Sum1_m + rtb_Sum2;

        /* Update for UnitDelay: '<S12>/UnitDelay1' */
        AbstractFuelControl_M1_DW.UnitDelay1_DSTATE = rtb_Sum2;
      }

      /* End of Logic: '<S8>/Logical Operator2' */
      /* End of Outputs for SubSystem: '<S8>/feedback_PI_controller' */

      /* Switch: '<S8>/Switch' incorporates:
       *  Constant: '<S8>/Constant3'
       *  DataStoreRead: '<S8>/DataStoreRead3'
       */
      if (AbstractFuelControl_M1_DW.controller_mode) {
        rtb_Sum1_m = 1.0F;
      } else {
        /* Sum: '<S8>/Sum1' incorporates:
         *  Constant: '<S8>/Constant2'
         */
        rtb_Sum1_m = 1.0F + AbstractFuelControl_M1_B.Sum3;

        /* Saturate: '<S8>/fb_fuel_saturation' */
        if ((rtb_Sum1_m <= 0.0F) || rtIsNaNF(0.0F)) {
          rtb_Sum1_m = 0.0F;
        }

        /* End of Saturate: '<S8>/fb_fuel_saturation' */
      }

      /* End of Switch: '<S8>/Switch' */

      /* Outputs for Atomic SubSystem: '<S8>/feedforward_controller' */
      /* Product: '<S8>/Prod1' incorporates:
       *  DataStoreRead: '<S8>/DataStoreRead4'
       *  Product: '<S13>/Product'
       */
      rtb_Sum1_m *= rtb_Sum_o / AbstractFuelControl_M1_DW.airbyfuel_ref;

      /* End of Outputs for SubSystem: '<S8>/feedforward_controller' */

      /* Saturate: '<S8>/fuel_saturation' */
      if (rtb_Sum1_m > 1.66F) {
        /* DataStoreWrite: '<S8>/DataStoreWrite' */
        AbstractFuelControl_M1_DW.commanded_fuel = 1.66F;
      } else if (rtb_Sum1_m < 0.13F) {
        /* DataStoreWrite: '<S8>/DataStoreWrite' */
        AbstractFuelControl_M1_DW.commanded_fuel = 0.13F;
      } else {
        /* DataStoreWrite: '<S8>/DataStoreWrite' */
        AbstractFuelControl_M1_DW.commanded_fuel = rtb_Sum1_m;
      }

      /* End of Saturate: '<S8>/fuel_saturation' */
    }
  }

  /* End of Outputs for SubSystem: '<S7>/fuel_controller_pwon' */
  /* End of Outputs for SubSystem: '<S7>/fuel_controller_mode_10ms' */
  /* End of Outputs for SubSystem: '<S7>/fuel_controller_10ms' */
  /* End of Outputs for SubSystem: '<S2>/fuel_controller' */
  if (rtmIsMajorTimeStep(AbstractFuelControl_M1_M) &&
      AbstractFuelControl_M1_M->Timing.TaskCounters.TID[2] == 0) {
    /* DataStoreRead: '<S2>/DataStoreRead' */
    rtb_DataStoreRead = AbstractFuelControl_M1_DW.commanded_fuel;

    /* DataStoreRead: '<S2>/DataStoreRead1' */
    rtb_DataStoreRead1_l = AbstractFuelControl_M1_DW.controller_mode;

    /* DataStoreRead: '<S2>/DataStoreRead2' */
    rtb_DataStoreRead2 = AbstractFuelControl_M1_DW.airbyfuel_ref;
  }

  /* End of Outputs for SubSystem: '<S1>/AF_Controller' */

  /* Integrator: '<S18>/Integrator' */
  rtb_Integrator = AbstractFuelControl_M1_X.Integrator_CSTATE_h;

  /* Gain: '<S19>/Gain' incorporates:
   *  Integrator: '<S18>/Integrator'
   *  Integrator: '<S19>/Integrator'
   *  Sum: '<S19>/Sum'
   */
  AbstractFuelControl_M1_B.Gain = (AbstractFuelControl_M1_X.Integrator_CSTATE_h
    - AbstractFuelControl_M1_X.Integrator_CSTATE) * 50.0;

  /* Fcn: '<S4>/Pumping' */
  rtb_fuelsystemtransportdelay = ((0.08979 * rtb_fuelsystemtransportdelay *
    rtb_radstorpm + -0.366) - 0.0337 * rtb_radstorpm *
    rtb_fuelsystemtransportdelay * rtb_fuelsystemtransportdelay) + 0.0001 *
    rtb_fuelsystemtransportdelay * rtb_radstorpm * rtb_radstorpm;

  /* Gain: '<S4>/Gain2' */
  rtb_PulseGenerator_10ms = 1.0F * rtb_fuelsystemtransportdelay;

  /* Gain: '<S6>/rad//s to rpm' */
  rtb_fuelsystemtransportdelay = 9.5492965855137211 * rtb_radstorpm;

  /* Gain: '<S3>/Gain' incorporates:
   *  Product: '<S3>/Product1'
   */
  rtb_ftheta = rtb_PulseGenerator_10ms / rtb_radstorpm * 3.1415926535897931;

  /* Gain: '<S6>/Kappa tolerance [0.9 1.1]' incorporates:
   *  Lookup_n-D: '<S6>/1-Kappa'
   */
  rtb_Kappatolerance0911 = 1.0 * look2_binlxpw(rtb_fuelsystemtransportdelay,
    rtb_ftheta, AbstractFuelControl_M1_ConstP.pooled9,
    AbstractFuelControl_M1_ConstP.pooled10,
    AbstractFuelControl_M1_ConstP.uKappa_tableData,
    AbstractFuelControl_M1_ConstP.pooled16, 5U);
  if (rtmIsMajorTimeStep(AbstractFuelControl_M1_M) &&
      AbstractFuelControl_M1_M->Timing.TaskCounters.TID[2] == 0) {
    /* Gain: '<S1>/fuel injector tolerance [0.95 1.05]' incorporates:
     *  DataTypeConversion: '<S1>/Data Type Conversion'
     */
    AbstractFuelControl_M1_B.fuelinjectortolerance095105 = 1.0 *
      rtb_DataStoreRead;
  }

  /* Lookup_n-D: '<S6>/tau_ww' */
  rtb_fuelsystemtransportdelay = look2_binlxpw(rtb_fuelsystemtransportdelay,
    rtb_ftheta, AbstractFuelControl_M1_ConstP.pooled9,
    AbstractFuelControl_M1_ConstP.pooled10,
    AbstractFuelControl_M1_ConstP.tau_ww_tableData,
    AbstractFuelControl_M1_ConstP.pooled16, 5U);

  /* Product: '<S6>/Divide2' incorporates:
   *  Gain: '<S6>/tau_ww tolerance [0.9 1.1]'
   *  Integrator: '<S6>/Integrator'
   */
  rtb_fuel_puddle_evap = AbstractFuelControl_M1_X.Integrator_CSTATE_c / (1.0 *
    rtb_fuelsystemtransportdelay);

  /* Product: '<S3>/Divide' incorporates:
   *  Product: '<S6>/Divide'
   *  Sum: '<S6>/Add'
   */
  AbstractFuelControl_M1_B.airbyfuel = rtb_PulseGenerator_10ms /
    (rtb_Kappatolerance0911 *
     AbstractFuelControl_M1_B.fuelinjectortolerance095105 + rtb_fuel_puddle_evap);

  /* VariableTransportDelay: '<S3>/fuel system transport delay' */
  {
    real_T **uBuffer = (real_T**)
      &AbstractFuelControl_M1_DW.fuelsystemtransportdelay_PWORK.TUbufferPtrs[0];
    real_T **tBuffer = (real_T**)
      &AbstractFuelControl_M1_DW.fuelsystemtransportdelay_PWORK.TUbufferPtrs[1];
    real_T **xBuffer = (real_T**)
      &AbstractFuelControl_M1_DW.fuelsystemtransportdelay_PWORK.TUbufferPtrs[2];
    real_T simTime = AbstractFuelControl_M1_M->Timing.t[0];
    real_T appliedDelay;

    /* For variable transport dealy, find the real applied dealy
     * here and then output
     */
    rtb_fuelsystemtransportdelay= rt_VTDelayfindtDInterpolate
      (AbstractFuelControl_M1_X.fuelsystemtransportdelay_CSTATE,*tBuffer,
       *uBuffer, *xBuffer,
       AbstractFuelControl_M1_DW.fuelsystemtransportdelay_IWORK.CircularBufSize,
       AbstractFuelControl_M1_DW.fuelsystemtransportdelay_IWORK.Head,
       AbstractFuelControl_M1_DW.fuelsystemtransportdelay_IWORK.Tail,
       &AbstractFuelControl_M1_DW.fuelsystemtransportdelay_IWORK.Last, simTime,
       0.0,0,
       0, 14.7,
       &appliedDelay);
  }

  /* Gain: '<S18>/Gain1' incorporates:
   *  Sum: '<S18>/Sum'
   */
  AbstractFuelControl_M1_B.Gain1 = (rtb_fuelsystemtransportdelay -
    rtb_Integrator) * 10.0;

  /* Gain: '<S3>/rad//s to rpm' */
  rtb_radstorpm *= 9.5492965855137211;

  /* Lookup_n-D: '<S3>/delay (s)' */
  AbstractFuelControl_M1_B.delays = look2_binlxpw(rtb_radstorpm, rtb_ftheta,
    AbstractFuelControl_M1_ConstP.delays_bp01Data,
    AbstractFuelControl_M1_ConstP.delays_bp02Data,
    AbstractFuelControl_M1_ConstP.delays_tableData,
    AbstractFuelControl_M1_ConstP.pooled16, 5U);

  /* Gain: '<S4>/RT//Vm' incorporates:
   *  Sum: '<S4>/Sum'
   */
  AbstractFuelControl_M1_B.RTVm = (rtb_pratio - rtb_PulseGenerator_10ms) *
    0.41328;

  /* Sum: '<S6>/Sum' incorporates:
   *  Constant: '<S6>/Constant'
   *  Gain: '<S6>/Gain'
   *  Product: '<S6>/Divide1'
   *  Sum: '<S6>/Add2'
   */
  AbstractFuelControl_M1_B.Sum = ((-1.0) * rtb_Kappatolerance0911 + 1.0) *
    AbstractFuelControl_M1_B.fuelinjectortolerance095105 - rtb_fuel_puddle_evap;

  /* End of Outputs for SubSystem: '<Root>/Model 1' */
  if (rtmIsMajorTimeStep(AbstractFuelControl_M1_M) &&
      AbstractFuelControl_M1_M->Timing.TaskCounters.TID[2] == 0) {
    /* Outport: '<Root>/AFref' incorporates:
     *  DataTypeConversion: '<Root>/Data Type Conversion'
     */
    AbstractFuelControl_M1_Y.AFref = rtb_DataStoreRead2;

    /* Outport: '<Root>/controller_mode' incorporates:
     *  DataTypeConversion: '<Root>/Data Type Conversion1'
     */
    AbstractFuelControl_M1_Y.controller_mode = rtb_DataStoreRead1_l;
  }

  /* Outport: '<Root>/AF' */
  AbstractFuelControl_M1_Y.AF = rtb_AF_sensor;
  if (rtmIsMajorTimeStep(AbstractFuelControl_M1_M)) {
    /* Update for Atomic SubSystem: '<Root>/Model 1' */

    /* Update for VariableTransportDelay: '<S3>/fuel system transport delay' */
    {
      real_T **uBuffer = (real_T**)
        &AbstractFuelControl_M1_DW.fuelsystemtransportdelay_PWORK.TUbufferPtrs[0];
      real_T **tBuffer = (real_T**)
        &AbstractFuelControl_M1_DW.fuelsystemtransportdelay_PWORK.TUbufferPtrs[1];
      real_T **xBuffer = (real_T**)
        &AbstractFuelControl_M1_DW.fuelsystemtransportdelay_PWORK.TUbufferPtrs[2];
      real_T simTime = AbstractFuelControl_M1_M->Timing.t[0];
      real_T appliedDelay;
      appliedDelay = AbstractFuelControl_M1_B.delays;
      if (appliedDelay > 10.0) {
        appliedDelay = 10.0;
      }

      AbstractFuelControl_M1_DW.fuelsystemtransportdelay_IWORK.Head =
        ((AbstractFuelControl_M1_DW.fuelsystemtransportdelay_IWORK.Head <
          (AbstractFuelControl_M1_DW.fuelsystemtransportdelay_IWORK.CircularBufSize
           -1)) ? (AbstractFuelControl_M1_DW.fuelsystemtransportdelay_IWORK.Head
                   +1) : 0);
      if (AbstractFuelControl_M1_DW.fuelsystemtransportdelay_IWORK.Head ==
          AbstractFuelControl_M1_DW.fuelsystemtransportdelay_IWORK.Tail) {
        AbstractFuelControl_M1_DW.fuelsystemtransportdelay_IWORK.Tail =
          ((AbstractFuelControl_M1_DW.fuelsystemtransportdelay_IWORK.Tail <
            (AbstractFuelControl_M1_DW.fuelsystemtransportdelay_IWORK.CircularBufSize
             -1)) ?
           (AbstractFuelControl_M1_DW.fuelsystemtransportdelay_IWORK.Tail+1) : 0);
      }

      (*tBuffer)[AbstractFuelControl_M1_DW.fuelsystemtransportdelay_IWORK.Head] =
        simTime;
      (*uBuffer)[AbstractFuelControl_M1_DW.fuelsystemtransportdelay_IWORK.Head] =
        AbstractFuelControl_M1_B.airbyfuel;
      (*xBuffer)[AbstractFuelControl_M1_DW.fuelsystemtransportdelay_IWORK.Head] =
        AbstractFuelControl_M1_X.fuelsystemtransportdelay_CSTATE;

      /* when use fixed buffer, reset solver at when buffer is updated
       * to avoid output consistency fail.
       */
    }

    /* End of Update for SubSystem: '<Root>/Model 1' */
  }                                    /* end MajorTimeStep */

  if (rtmIsMajorTimeStep(AbstractFuelControl_M1_M)) {
    rt_ertODEUpdateContinuousStates(&AbstractFuelControl_M1_M->solverInfo);

    /* Update absolute time for base rate */
    /* The "clockTick0" counts the number of times the code of this task has
     * been executed. The absolute time is the multiplication of "clockTick0"
     * and "Timing.stepSize0". Size of "clockTick0" ensures timer will not
     * overflow during the application lifespan selected.
     */
    ++AbstractFuelControl_M1_M->Timing.clockTick0;
    AbstractFuelControl_M1_M->Timing.t[0] = rtsiGetSolverStopTime
      (&AbstractFuelControl_M1_M->solverInfo);

    {
      /* Update absolute timer for sample time: [0.005s, 0.0s] */
      /* The "clockTick1" counts the number of times the code of this task has
       * been executed. The resolution of this integer timer is 0.005, which is the step size
       * of the task. Size of "clockTick1" ensures timer will not overflow during the
       * application lifespan selected.
       */
      AbstractFuelControl_M1_M->Timing.clockTick1++;
    }

    rate_scheduler();
  }                                    /* end MajorTimeStep */
}

/* Derivatives for root system: '<Root>' */
void AbstractFuelControl_M1_derivatives(void)
{
  XDot_AbstractFuelControl_M1_T *_rtXdot;
  _rtXdot = ((XDot_AbstractFuelControl_M1_T *) AbstractFuelControl_M1_M->derivs);

  /* Derivatives for Atomic SubSystem: '<Root>/Model 1' */
  /* Derivatives for Integrator: '<S19>/Integrator' */
  _rtXdot->Integrator_CSTATE = AbstractFuelControl_M1_B.Gain;

  /* Derivatives for TransferFcn: '<S1>/Throttle delay' incorporates:
   *  Derivatives for Inport: '<Root>/Pedal Angle'
   */
  _rtXdot->Throttledelay_CSTATE = 0.0;
  _rtXdot->Throttledelay_CSTATE += (-10.0) *
    AbstractFuelControl_M1_X.Throttledelay_CSTATE;
  _rtXdot->Throttledelay_CSTATE += AbstractFuelControl_M1_U.PedalAngle;

  /* Derivatives for Integrator: '<S4>/p0 = 0.543 (bar)' */
  _rtXdot->p00543bar_CSTATE = AbstractFuelControl_M1_B.RTVm;

  /* Derivatives for Integrator: '<S18>/Integrator' */
  _rtXdot->Integrator_CSTATE_h = AbstractFuelControl_M1_B.Gain1;

  /* Derivatives for Integrator: '<S6>/Integrator' */
  _rtXdot->Integrator_CSTATE_c = AbstractFuelControl_M1_B.Sum;

  /* Derivatives for VariableTransportDelay: '<S3>/fuel system transport delay' */
  {
    real_T instantDelay;
    instantDelay = AbstractFuelControl_M1_B.delays;
    if (instantDelay > 10.0) {
      instantDelay = 10.0;
    }

    if (instantDelay < 0.0) {
      ((XDot_AbstractFuelControl_M1_T *) AbstractFuelControl_M1_M->derivs)
        ->fuelsystemtransportdelay_CSTATE = 0;
    } else {
      ((XDot_AbstractFuelControl_M1_T *) AbstractFuelControl_M1_M->derivs)
        ->fuelsystemtransportdelay_CSTATE = 1.0/instantDelay;
    }
  }

  /* End of Derivatives for SubSystem: '<Root>/Model 1' */
}

/* Model initialize function */
void AbstractFuelControl_M1_initialize(void)
{
  /* Registration code */

  /* initialize non-finites */
  rt_InitInfAndNaN(sizeof(real_T));

  /* initialize real-time model */
  (void) memset((void *)AbstractFuelControl_M1_M, 0,
                sizeof(RT_MODEL_AbstractFuelControl__T));

  {
    /* Setup solver object */
    rtsiSetSimTimeStepPtr(&AbstractFuelControl_M1_M->solverInfo,
                          &AbstractFuelControl_M1_M->Timing.simTimeStep);
    rtsiSetTPtr(&AbstractFuelControl_M1_M->solverInfo, &rtmGetTPtr
                (AbstractFuelControl_M1_M));
    rtsiSetStepSizePtr(&AbstractFuelControl_M1_M->solverInfo,
                       &AbstractFuelControl_M1_M->Timing.stepSize0);
    rtsiSetdXPtr(&AbstractFuelControl_M1_M->solverInfo,
                 &AbstractFuelControl_M1_M->derivs);
    rtsiSetContStatesPtr(&AbstractFuelControl_M1_M->solverInfo, (real_T **)
                         &AbstractFuelControl_M1_M->contStates);
    rtsiSetNumContStatesPtr(&AbstractFuelControl_M1_M->solverInfo,
      &AbstractFuelControl_M1_M->Sizes.numContStates);
    rtsiSetNumPeriodicContStatesPtr(&AbstractFuelControl_M1_M->solverInfo,
      &AbstractFuelControl_M1_M->Sizes.numPeriodicContStates);
    rtsiSetPeriodicContStateIndicesPtr(&AbstractFuelControl_M1_M->solverInfo,
      &AbstractFuelControl_M1_M->periodicContStateIndices);
    rtsiSetPeriodicContStateRangesPtr(&AbstractFuelControl_M1_M->solverInfo,
      &AbstractFuelControl_M1_M->periodicContStateRanges);
    rtsiSetErrorStatusPtr(&AbstractFuelControl_M1_M->solverInfo,
                          (&rtmGetErrorStatus(AbstractFuelControl_M1_M)));
    rtsiSetRTModelPtr(&AbstractFuelControl_M1_M->solverInfo,
                      AbstractFuelControl_M1_M);
  }

  rtsiSetSimTimeStep(&AbstractFuelControl_M1_M->solverInfo, MAJOR_TIME_STEP);
  AbstractFuelControl_M1_M->intgData.y = AbstractFuelControl_M1_M->odeY;
  AbstractFuelControl_M1_M->intgData.f[0] = AbstractFuelControl_M1_M->odeF[0];
  AbstractFuelControl_M1_M->intgData.f[1] = AbstractFuelControl_M1_M->odeF[1];
  AbstractFuelControl_M1_M->intgData.f[2] = AbstractFuelControl_M1_M->odeF[2];
  AbstractFuelControl_M1_M->contStates = ((X_AbstractFuelControl_M1_T *)
    &AbstractFuelControl_M1_X);
  rtsiSetSolverData(&AbstractFuelControl_M1_M->solverInfo, (void *)
                    &AbstractFuelControl_M1_M->intgData);
  rtsiSetSolverName(&AbstractFuelControl_M1_M->solverInfo,"ode3");
  rtmSetTPtr(AbstractFuelControl_M1_M, &AbstractFuelControl_M1_M->Timing.tArray
             [0]);
  AbstractFuelControl_M1_M->Timing.stepSize0 = 0.005;

  /* block I/O */
  (void) memset(((void *) &AbstractFuelControl_M1_B), 0,
                sizeof(B_AbstractFuelControl_M1_T));

  /* states (continuous) */
  {
    (void) memset((void *)&AbstractFuelControl_M1_X, 0,
                  sizeof(X_AbstractFuelControl_M1_T));
  }

  /* states (dwork) */
  (void) memset((void *)&AbstractFuelControl_M1_DW, 0,
                sizeof(DW_AbstractFuelControl_M1_T));

  /* external inputs */
  (void)memset((void *)&AbstractFuelControl_M1_U, 0, sizeof
               (ExtU_AbstractFuelControl_M1_T));

  /* external outputs */
  (void) memset((void *)&AbstractFuelControl_M1_Y, 0,
                sizeof(ExtY_AbstractFuelControl_M1_T));

  /* Start for Atomic SubSystem: '<Root>/Model 1' */
  /* Start for Atomic SubSystem: '<S1>/AF_Controller' */
  /* Start for DiscretePulseGenerator: '<S2>/PulseGenerator_10ms' */
  AbstractFuelControl_M1_DW.clockTickCounter = -2;

  /* Start for DataStoreMemory: '<S2>/commanded_fuel' */
  AbstractFuelControl_M1_DW.commanded_fuel = 0.1726F;

  /* End of Start for SubSystem: '<S1>/AF_Controller' */
  /* Start for VariableTransportDelay: '<S3>/fuel system transport delay' */
  {
    real_T *pBuffer =
      &AbstractFuelControl_M1_DW.fuelsystemtransportdelay_RWORK.TUbufferArea[0];
    int_T j;
    AbstractFuelControl_M1_DW.fuelsystemtransportdelay_IWORK.Tail = 0;
    AbstractFuelControl_M1_DW.fuelsystemtransportdelay_IWORK.Head = 0;
    AbstractFuelControl_M1_DW.fuelsystemtransportdelay_IWORK.Last = 0;
    AbstractFuelControl_M1_DW.fuelsystemtransportdelay_IWORK.CircularBufSize =
      20480;
    for (j=0; j < 20480; j++) {
      pBuffer[j] = 14.7;
      pBuffer[20480 + j] = AbstractFuelControl_M1_M->Timing.t[0];
    }

    pBuffer[2*20480] = 0.0;
    AbstractFuelControl_M1_DW.fuelsystemtransportdelay_PWORK.TUbufferPtrs[0] =
      (void *) &pBuffer[0];
    AbstractFuelControl_M1_DW.fuelsystemtransportdelay_PWORK.TUbufferPtrs[1] =
      (void *) &pBuffer[20480];
    AbstractFuelControl_M1_DW.fuelsystemtransportdelay_PWORK.TUbufferPtrs[2] =
      (void *) &pBuffer[2*20480];
  }

  /* End of Start for SubSystem: '<Root>/Model 1' */
  AbstractFuelControl_M1_PrevZCX.fuel_controller_10ms_Trig_ZCE =
    UNINITIALIZED_ZCSIG;
  AbstractFuelControl_M1_PrevZCX.fuel_controller_mode_10ms_Trig_ =
    UNINITIALIZED_ZCSIG;
  AbstractFuelControl_M1_PrevZCX.fuel_controller_pwon_Trig_ZCE =
    UNINITIALIZED_ZCSIG;

  /* SystemInitialize for Atomic SubSystem: '<Root>/Model 1' */
  /* InitializeConditions for Integrator: '<S19>/Integrator' */
  AbstractFuelControl_M1_X.Integrator_CSTATE = 14.7;

  /* InitializeConditions for TransferFcn: '<S1>/Throttle delay' */
  AbstractFuelControl_M1_X.Throttledelay_CSTATE = 0.0;

  /* InitializeConditions for Integrator: '<S4>/p0 = 0.543 (bar)' */
  AbstractFuelControl_M1_X.p00543bar_CSTATE = 0.982;

  /* InitializeConditions for Integrator: '<S18>/Integrator' */
  AbstractFuelControl_M1_X.Integrator_CSTATE_h = 14.7;

  /* InitializeConditions for Integrator: '<S6>/Integrator' */
  AbstractFuelControl_M1_X.Integrator_CSTATE_c = 0.0112;

  /* InitializeConditions for VariableTransportDelay: '<S3>/fuel system transport delay' */
  AbstractFuelControl_M1_X.fuelsystemtransportdelay_CSTATE = 0.0;

  /* SystemInitialize for Atomic SubSystem: '<S1>/AF_Controller' */
  /* SystemInitialize for Atomic SubSystem: '<S2>/fuel_controller' */
  /* SystemInitialize for Triggered SubSystem: '<S7>/fuel_controller_10ms' */
  /* SystemInitialize for Atomic SubSystem: '<S8>/air_estimation' */
  /* InitializeConditions for UnitDelay: '<S11>/UnitDelay1' */
  AbstractFuelControl_M1_DW.UnitDelay1_DSTATE_d = 0.982F;

  /* End of SystemInitialize for SubSystem: '<S8>/air_estimation' */
  /* End of SystemInitialize for SubSystem: '<S7>/fuel_controller_10ms' */
  /* End of SystemInitialize for SubSystem: '<S2>/fuel_controller' */
  /* End of SystemInitialize for SubSystem: '<S1>/AF_Controller' */
  /* End of SystemInitialize for SubSystem: '<Root>/Model 1' */
}

/* Model terminate function */
void AbstractFuelControl_M1_terminate(void)
{
  /* (no terminate code required) */
}

/*
 * File trailer for generated code.
 *
 * [EOF]
 */
