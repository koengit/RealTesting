/*
 * Academic License - for use in teaching, academic research, and meeting
 * course requirements at degree granting institutions only.  Not for
 * government, commercial, or other organizational use.
 *
 * File: Autotrans_shift.c
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

/* Named constants for Chart: '<Root>/ShiftLogic' */
#define Autotrans_sh_IN_NO_ACTIVE_CHILD ((uint8_T)0U)
#define Autotrans_shift_CALL_EVENT     (-1)
#define Autotrans_shift_IN_downshifting ((uint8_T)1U)
#define Autotrans_shift_IN_first       ((uint8_T)1U)
#define Autotrans_shift_IN_fourth      ((uint8_T)2U)
#define Autotrans_shift_IN_second      ((uint8_T)3U)
#define Autotrans_shift_IN_steady_state ((uint8_T)2U)
#define Autotrans_shift_IN_third       ((uint8_T)4U)
#define Autotrans_shift_IN_upshifting  ((uint8_T)3U)
#define Autotrans_shift_event_DOWN     (0)
#define Autotrans_shift_event_UP       (1)

/* Block signals (auto storage) */
B_Autotrans_shift_T Autotrans_shift_B;

/* Continuous states */
X_Autotrans_shift_T Autotrans_shift_X;

/* Block states (auto storage) */
DW_Autotrans_shift_T Autotrans_shift_DW;

/* External inputs (root inport signals with auto storage) */
ExtU_Autotrans_shift_T Autotrans_shift_U;

/* External outputs (root outports fed by signals with auto storage) */
ExtY_Autotrans_shift_T Autotrans_shift_Y;

/* Real-time model */
RT_MODEL_Autotrans_shift_T Autotrans_shift_M_;
RT_MODEL_Autotrans_shift_T *const Autotrans_shift_M = &Autotrans_shift_M_;

/* Forward declaration for local functions */
static void Autotrans_shift_gear_state(const int32_T *sfEvent);
static void rate_scheduler(void);

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
  (Autotrans_shift_M->Timing.TaskCounters.TID[2])++;
  if ((Autotrans_shift_M->Timing.TaskCounters.TID[2]) > 3) {/* Sample time: [0.04s, 0.0s] */
    Autotrans_shift_M->Timing.TaskCounters.TID[2] = 0;
  }
}

/*
 * This function updates continuous states using the ODE5 fixed-step
 * solver algorithm
 */
static void rt_ertODEUpdateContinuousStates(RTWSolverInfo *si )
{
  /* Solver Matrices */
  static const real_T rt_ODE5_A[6] = {
    1.0/5.0, 3.0/10.0, 4.0/5.0, 8.0/9.0, 1.0, 1.0
  };

  static const real_T rt_ODE5_B[6][6] = {
    { 1.0/5.0, 0.0, 0.0, 0.0, 0.0, 0.0 },

    { 3.0/40.0, 9.0/40.0, 0.0, 0.0, 0.0, 0.0 },

    { 44.0/45.0, -56.0/15.0, 32.0/9.0, 0.0, 0.0, 0.0 },

    { 19372.0/6561.0, -25360.0/2187.0, 64448.0/6561.0, -212.0/729.0, 0.0, 0.0 },

    { 9017.0/3168.0, -355.0/33.0, 46732.0/5247.0, 49.0/176.0, -5103.0/18656.0,
      0.0 },

    { 35.0/384.0, 0.0, 500.0/1113.0, 125.0/192.0, -2187.0/6784.0, 11.0/84.0 }
  };

  time_T t = rtsiGetT(si);
  time_T tnew = rtsiGetSolverStopTime(si);
  time_T h = rtsiGetStepSize(si);
  real_T *x = rtsiGetContStates(si);
  ODE5_IntgData *id = (ODE5_IntgData *)rtsiGetSolverData(si);
  real_T *y = id->y;
  real_T *f0 = id->f[0];
  real_T *f1 = id->f[1];
  real_T *f2 = id->f[2];
  real_T *f3 = id->f[3];
  real_T *f4 = id->f[4];
  real_T *f5 = id->f[5];
  real_T hB[6];
  int_T i;
  int_T nXc = 2;
  rtsiSetSimTimeStep(si,MINOR_TIME_STEP);

  /* Save the state values at time t in y, we'll use x as ynew. */
  (void) memcpy(y, x,
                (uint_T)nXc*sizeof(real_T));

  /* Assumes that rtsiSetT and ModelOutputs are up-to-date */
  /* f0 = f(t,y) */
  rtsiSetdX(si, f0);
  Autotrans_shift_derivatives();

  /* f(:,2) = feval(odefile, t + hA(1), y + f*hB(:,1), args(:)(*)); */
  hB[0] = h * rt_ODE5_B[0][0];
  for (i = 0; i < nXc; i++) {
    x[i] = y[i] + (f0[i]*hB[0]);
  }

  rtsiSetT(si, t + h*rt_ODE5_A[0]);
  rtsiSetdX(si, f1);
  Autotrans_shift_step();
  Autotrans_shift_derivatives();

  /* f(:,3) = feval(odefile, t + hA(2), y + f*hB(:,2), args(:)(*)); */
  for (i = 0; i <= 1; i++) {
    hB[i] = h * rt_ODE5_B[1][i];
  }

  for (i = 0; i < nXc; i++) {
    x[i] = y[i] + (f0[i]*hB[0] + f1[i]*hB[1]);
  }

  rtsiSetT(si, t + h*rt_ODE5_A[1]);
  rtsiSetdX(si, f2);
  Autotrans_shift_step();
  Autotrans_shift_derivatives();

  /* f(:,4) = feval(odefile, t + hA(3), y + f*hB(:,3), args(:)(*)); */
  for (i = 0; i <= 2; i++) {
    hB[i] = h * rt_ODE5_B[2][i];
  }

  for (i = 0; i < nXc; i++) {
    x[i] = y[i] + (f0[i]*hB[0] + f1[i]*hB[1] + f2[i]*hB[2]);
  }

  rtsiSetT(si, t + h*rt_ODE5_A[2]);
  rtsiSetdX(si, f3);
  Autotrans_shift_step();
  Autotrans_shift_derivatives();

  /* f(:,5) = feval(odefile, t + hA(4), y + f*hB(:,4), args(:)(*)); */
  for (i = 0; i <= 3; i++) {
    hB[i] = h * rt_ODE5_B[3][i];
  }

  for (i = 0; i < nXc; i++) {
    x[i] = y[i] + (f0[i]*hB[0] + f1[i]*hB[1] + f2[i]*hB[2] +
                   f3[i]*hB[3]);
  }

  rtsiSetT(si, t + h*rt_ODE5_A[3]);
  rtsiSetdX(si, f4);
  Autotrans_shift_step();
  Autotrans_shift_derivatives();

  /* f(:,6) = feval(odefile, t + hA(5), y + f*hB(:,5), args(:)(*)); */
  for (i = 0; i <= 4; i++) {
    hB[i] = h * rt_ODE5_B[4][i];
  }

  for (i = 0; i < nXc; i++) {
    x[i] = y[i] + (f0[i]*hB[0] + f1[i]*hB[1] + f2[i]*hB[2] +
                   f3[i]*hB[3] + f4[i]*hB[4]);
  }

  rtsiSetT(si, tnew);
  rtsiSetdX(si, f5);
  Autotrans_shift_step();
  Autotrans_shift_derivatives();

  /* tnew = t + hA(6);
     ynew = y + f*hB(:,6); */
  for (i = 0; i <= 5; i++) {
    hB[i] = h * rt_ODE5_B[5][i];
  }

  for (i = 0; i < nXc; i++) {
    x[i] = y[i] + (f0[i]*hB[0] + f1[i]*hB[1] + f2[i]*hB[2] +
                   f3[i]*hB[3] + f4[i]*hB[4] + f5[i]*hB[5]);
  }

  rtsiSetSimTimeStep(si,MAJOR_TIME_STEP);
}

/* Function for Chart: '<Root>/ShiftLogic' */
static void Autotrans_shift_gear_state(const int32_T *sfEvent)
{
  /* During 'gear_state': '<S2>:2' */
  switch (Autotrans_shift_DW.is_gear_state) {
   case Autotrans_shift_IN_first:
    /* During 'first': '<S2>:6' */
    if (*sfEvent == Autotrans_shift_event_UP) {
      /* Transition: '<S2>:12' */
      Autotrans_shift_DW.is_gear_state = Autotrans_shift_IN_second;

      /* Entry 'second': '<S2>:4' */
      Autotrans_shift_B.gear = 2.0;
    }
    break;

   case Autotrans_shift_IN_fourth:
    /* During 'fourth': '<S2>:3' */
    if (*sfEvent == Autotrans_shift_event_DOWN) {
      /* Transition: '<S2>:14' */
      Autotrans_shift_DW.is_gear_state = Autotrans_shift_IN_third;

      /* Entry 'third': '<S2>:5' */
      Autotrans_shift_B.gear = 3.0;
    }
    break;

   case Autotrans_shift_IN_second:
    /* During 'second': '<S2>:4' */
    switch (*sfEvent) {
     case Autotrans_shift_event_UP:
      /* Transition: '<S2>:11' */
      Autotrans_shift_DW.is_gear_state = Autotrans_shift_IN_third;

      /* Entry 'third': '<S2>:5' */
      Autotrans_shift_B.gear = 3.0;
      break;

     case Autotrans_shift_event_DOWN:
      /* Transition: '<S2>:16' */
      Autotrans_shift_DW.is_gear_state = Autotrans_shift_IN_first;

      /* Entry 'first': '<S2>:6' */
      Autotrans_shift_B.gear = 1.0;
      break;
    }
    break;

   case Autotrans_shift_IN_third:
    /* During 'third': '<S2>:5' */
    switch (*sfEvent) {
     case Autotrans_shift_event_UP:
      /* Transition: '<S2>:10' */
      Autotrans_shift_DW.is_gear_state = Autotrans_shift_IN_fourth;

      /* Entry 'fourth': '<S2>:3' */
      Autotrans_shift_B.gear = 4.0;
      break;

     case Autotrans_shift_event_DOWN:
      /* Transition: '<S2>:15' */
      Autotrans_shift_DW.is_gear_state = Autotrans_shift_IN_second;

      /* Entry 'second': '<S2>:4' */
      Autotrans_shift_B.gear = 2.0;
      break;
    }
    break;
  }
}

real_T rt_powd_snf(real_T u0, real_T u1)
{
  real_T y;
  real_T tmp;
  real_T tmp_0;
  if (rtIsNaN(u0) || rtIsNaN(u1)) {
    y = (rtNaN);
  } else {
    tmp = fabs(u0);
    tmp_0 = fabs(u1);
    if (rtIsInf(u1)) {
      if (tmp == 1.0) {
        y = (rtNaN);
      } else if (tmp > 1.0) {
        if (u1 > 0.0) {
          y = (rtInf);
        } else {
          y = 0.0;
        }
      } else if (u1 > 0.0) {
        y = 0.0;
      } else {
        y = (rtInf);
      }
    } else if (tmp_0 == 0.0) {
      y = 1.0;
    } else if (tmp_0 == 1.0) {
      if (u1 > 0.0) {
        y = u0;
      } else {
        y = 1.0 / u0;
      }
    } else if (u1 == 2.0) {
      y = u0 * u0;
    } else if ((u1 == 0.5) && (u0 >= 0.0)) {
      y = sqrt(u0);
    } else if ((u0 < 0.0) && (u1 > floor(u1))) {
      y = (rtNaN);
    } else {
      y = pow(u0, u1);
    }
  }

  return y;
}

/* Model step function */
void Autotrans_shift_step(void)
{
  real_T rtb_SpeedRatio;
  real_T rtb_Quotient;
  int32_T sfEvent;
  if (rtmIsMajorTimeStep(Autotrans_shift_M)) {
    /* set solver stop time */
    rtsiSetSolverStopTime(&Autotrans_shift_M->solverInfo,
                          ((Autotrans_shift_M->Timing.clockTick0+1)*
      Autotrans_shift_M->Timing.stepSize0));
  }                                    /* end MajorTimeStep */

  /* Update absolute time of base rate at minor time step */
  if (rtmIsMinorTimeStep(Autotrans_shift_M)) {
    Autotrans_shift_M->Timing.t[0] = rtsiGetT(&Autotrans_shift_M->solverInfo);
  }

  /* Gain: '<S5>/mph' incorporates:
   *  Gain: '<S5>/LinearSpeed'
   *  Integrator: '<S5>/Wheel Speed'
   */
  Autotrans_shift_B.VehicleSpeed = 6.2831853071795862 *
    Autotrans_shift_X.WheelSpeed_CSTATE * 0.011363636363636364;

  /* Outport: '<Root>/speed' */
  Autotrans_shift_Y.speed = Autotrans_shift_B.VehicleSpeed;

  /* Integrator: '<S1>/Integrator' */
  /* Limited  Integrator  */
  if (Autotrans_shift_X.Integrator_CSTATE >= 6000.0) {
    Autotrans_shift_X.Integrator_CSTATE = 6000.0;
  } else {
    if (Autotrans_shift_X.Integrator_CSTATE <= 600.0) {
      Autotrans_shift_X.Integrator_CSTATE = 600.0;
    }
  }

  Autotrans_shift_B.Integrator = Autotrans_shift_X.Integrator_CSTATE;

  /* End of Integrator: '<S1>/Integrator' */

  /* Outport: '<Root>/RPM' */
  Autotrans_shift_Y.RPM = Autotrans_shift_B.Integrator;
  if (rtmIsMajorTimeStep(Autotrans_shift_M) &&
      Autotrans_shift_M->Timing.TaskCounters.TID[2] == 0) {
    /* Chart: '<Root>/ShiftLogic' */
    /* Gateway: ShiftLogic */
    sfEvent = Autotrans_shift_CALL_EVENT;
    if (Autotrans_shift_DW.temporalCounter_i1 < MAX_uint32_T) {
      Autotrans_shift_DW.temporalCounter_i1++;
    }

    /* During: ShiftLogic */
    if (Autotrans_shift_DW.is_active_c1_Autotrans_shift == 0U) {
      /* Entry: ShiftLogic */
      Autotrans_shift_DW.is_active_c1_Autotrans_shift = 1U;

      /* Entry Internal: ShiftLogic */
      Autotrans_shift_DW.is_active_gear_state = 1U;

      /* Entry Internal 'gear_state': '<S2>:2' */
      /* Transition: '<S2>:13' */
      if (Autotrans_shift_DW.is_gear_state != Autotrans_shift_IN_first) {
        Autotrans_shift_DW.is_gear_state = Autotrans_shift_IN_first;

        /* Entry 'first': '<S2>:6' */
        Autotrans_shift_B.gear = 1.0;
      }

      Autotrans_shift_DW.is_active_selection_state = 1U;

      /* Entry Internal 'selection_state': '<S2>:7' */
      /* Transition: '<S2>:17' */
      Autotrans_shift_DW.is_selection_state = Autotrans_shift_IN_steady_state;
    } else {
      if (Autotrans_shift_DW.is_active_gear_state != 0U) {
        Autotrans_shift_gear_state(&sfEvent);
      }

      if (Autotrans_shift_DW.is_active_selection_state != 0U) {
        /* Outputs for Function Call SubSystem: '<Root>/ThresholdCalculation' */
        /* Lookup2D: '<S3>/interp_down' incorporates:
         *  Inport: '<Root>/throttle'
         */
        /* During 'selection_state': '<S2>:7' */
        /* Event: '<S2>:29' */
        Autotrans_shift_B.interp_down = rt_Lookup2D_Normal
          (Autotrans_shift_ConstP.interp_down_RowIdx, 6,
           Autotrans_shift_ConstP.pooled1, 4,
           Autotrans_shift_ConstP.interp_down_Table, Autotrans_shift_U.throttle,
           Autotrans_shift_B.gear);

        /* Lookup2D: '<S3>/interp_up' incorporates:
         *  Inport: '<Root>/throttle'
         */
        Autotrans_shift_B.interp_up = rt_Lookup2D_Normal
          (Autotrans_shift_ConstP.interp_up_RowIdx, 6,
           Autotrans_shift_ConstP.pooled1, 4,
           Autotrans_shift_ConstP.interp_up_Table, Autotrans_shift_U.throttle,
           Autotrans_shift_B.gear);

        /* End of Outputs for SubSystem: '<Root>/ThresholdCalculation' */
        switch (Autotrans_shift_DW.is_selection_state) {
         case Autotrans_shift_IN_downshifting:
          /* During 'downshifting': '<S2>:1' */
          if ((Autotrans_shift_DW.temporalCounter_i1 >= (uint32_T)2.0) &&
              (Autotrans_shift_B.VehicleSpeed <= Autotrans_shift_B.interp_down))
          {
            /* Transition: '<S2>:22' */
            /* Event: '<S2>:30' */
            sfEvent = Autotrans_shift_event_DOWN;
            if (Autotrans_shift_DW.is_active_gear_state != 0U) {
              Autotrans_shift_gear_state(&sfEvent);
            }

            Autotrans_shift_DW.is_selection_state =
              Autotrans_shift_IN_steady_state;
          } else {
            if (Autotrans_shift_B.VehicleSpeed > Autotrans_shift_B.interp_down)
            {
              /* Transition: '<S2>:21' */
              Autotrans_shift_DW.is_selection_state =
                Autotrans_shift_IN_steady_state;
            }
          }
          break;

         case Autotrans_shift_IN_steady_state:
          /* During 'steady_state': '<S2>:9' */
          if (Autotrans_shift_B.VehicleSpeed > Autotrans_shift_B.interp_up) {
            /* Transition: '<S2>:18' */
            Autotrans_shift_DW.is_selection_state =
              Autotrans_shift_IN_upshifting;
            Autotrans_shift_DW.temporalCounter_i1 = 0U;
          } else {
            if (Autotrans_shift_B.VehicleSpeed < Autotrans_shift_B.interp_down)
            {
              /* Transition: '<S2>:19' */
              Autotrans_shift_DW.is_selection_state =
                Autotrans_shift_IN_downshifting;
              Autotrans_shift_DW.temporalCounter_i1 = 0U;
            }
          }
          break;

         case Autotrans_shift_IN_upshifting:
          /* During 'upshifting': '<S2>:8' */
          if ((Autotrans_shift_DW.temporalCounter_i1 >= (uint32_T)2.0) &&
              (Autotrans_shift_B.VehicleSpeed >= Autotrans_shift_B.interp_up)) {
            /* Transition: '<S2>:23' */
            /* Event: '<S2>:31' */
            sfEvent = Autotrans_shift_event_UP;
            if (Autotrans_shift_DW.is_active_gear_state != 0U) {
              Autotrans_shift_gear_state(&sfEvent);
            }

            Autotrans_shift_DW.is_selection_state =
              Autotrans_shift_IN_steady_state;
          } else {
            if (Autotrans_shift_B.VehicleSpeed < Autotrans_shift_B.interp_up) {
              /* Transition: '<S2>:20' */
              Autotrans_shift_DW.is_selection_state =
                Autotrans_shift_IN_steady_state;
            }
          }
          break;
        }
      }
    }

    /* End of Chart: '<Root>/ShiftLogic' */

    /* Outport: '<Root>/gear' */
    Autotrans_shift_Y.gear = Autotrans_shift_B.gear;
  }

  if (rtmIsMajorTimeStep(Autotrans_shift_M) &&
      Autotrans_shift_M->Timing.TaskCounters.TID[1] == 0) {
  }

  if (rtmIsMajorTimeStep(Autotrans_shift_M) &&
      Autotrans_shift_M->Timing.TaskCounters.TID[2] == 0) {
    /* Lookup: '<S7>/Look-Up Table' */
    Autotrans_shift_B.LookUpTable = rt_Lookup(Autotrans_shift_ConstP.pooled1, 4,
      Autotrans_shift_B.gear, Autotrans_shift_ConstP.LookUpTable_YData);
  }

  /* Gain: '<S5>/FinalDriveRatio2' incorporates:
   *  Integrator: '<S5>/Wheel Speed'
   */
  Autotrans_shift_B.TransmissionRPM = 3.23 * Autotrans_shift_X.WheelSpeed_CSTATE;

  /* Product: '<S6>/SpeedRatio' incorporates:
   *  Product: '<S7>/Product1'
   */
  rtb_SpeedRatio = Autotrans_shift_B.LookUpTable *
    Autotrans_shift_B.TransmissionRPM / Autotrans_shift_B.Integrator;

  /* Product: '<S6>/Quotient' incorporates:
   *  Lookup: '<S6>/FactorK'
   */
  rtb_Quotient = Autotrans_shift_B.Integrator / rt_Lookup
    (Autotrans_shift_ConstP.pooled3, 21, rtb_SpeedRatio,
     Autotrans_shift_ConstP.FactorK_YData);

  /* Fcn: '<S6>/Impeller' */
  rtb_Quotient = rt_powd_snf(rtb_Quotient, 2.0);

  /* Gain: '<S1>/engine + impeller inertia' incorporates:
   *  Fcn: '<S6>/Impeller'
   *  Inport: '<Root>/throttle'
   *  Lookup2D: '<S1>/EngineTorque'
   *  Sum: '<S1>/Sum'
   */
  Autotrans_shift_B.engineimpellerinertia = (rt_Lookup2D_Normal
    (Autotrans_shift_ConstP.EngineTorque_RowIdx, 10,
     Autotrans_shift_ConstP.EngineTorque_ColIdx, 11,
     Autotrans_shift_ConstP.EngineTorque_Table, Autotrans_shift_U.throttle,
     Autotrans_shift_B.Integrator) - rtb_Quotient) * 45.472138452209627;

  /* Product: '<S7>/Product' incorporates:
   *  Fcn: '<S6>/Impeller'
   *  Lookup: '<S6>/TorqueRatio'
   *  Product: '<S6>/Turbine'
   */
  Autotrans_shift_B.OutputTorque = rtb_Quotient * rt_Lookup
    (Autotrans_shift_ConstP.pooled3, 21, rtb_SpeedRatio,
     Autotrans_shift_ConstP.TorqueRatio_YData) * Autotrans_shift_B.LookUpTable;

  /* Signum: '<S5>/Sign' */
  if (Autotrans_shift_B.VehicleSpeed < 0.0) {
    rtb_SpeedRatio = -1.0;
  } else if (Autotrans_shift_B.VehicleSpeed > 0.0) {
    rtb_SpeedRatio = 1.0;
  } else if (Autotrans_shift_B.VehicleSpeed == 0.0) {
    rtb_SpeedRatio = 0.0;
  } else {
    rtb_SpeedRatio = Autotrans_shift_B.VehicleSpeed;
  }

  /* End of Signum: '<S5>/Sign' */

  /* Gain: '<S5>/Vehicle Inertia' incorporates:
   *  Fcn: '<S5>/RoadLoad'
   *  Gain: '<S5>/Final Drive Ratio1'
   *  Inport: '<Root>/brake'
   *  Product: '<S5>/SignedLoad'
   *  Sum: '<S5>/Sum'
   *  Sum: '<S5>/Sum1'
   */
  Autotrans_shift_B.VehicleInertia = (3.23 * Autotrans_shift_B.OutputTorque -
    ((0.02 * rt_powd_snf(Autotrans_shift_B.VehicleSpeed, 2.0) + 40.0) +
     Autotrans_shift_U.brake) * rtb_SpeedRatio) * 0.082684618362373577;
  if (rtmIsMajorTimeStep(Autotrans_shift_M)) {
    rt_ertODEUpdateContinuousStates(&Autotrans_shift_M->solverInfo);

    /* Update absolute time for base rate */
    /* The "clockTick0" counts the number of times the code of this task has
     * been executed. The absolute time is the multiplication of "clockTick0"
     * and "Timing.stepSize0". Size of "clockTick0" ensures timer will not
     * overflow during the application lifespan selected.
     */
    ++Autotrans_shift_M->Timing.clockTick0;
    Autotrans_shift_M->Timing.t[0] = rtsiGetSolverStopTime
      (&Autotrans_shift_M->solverInfo);

    {
      /* Update absolute timer for sample time: [0.01s, 0.0s] */
      /* The "clockTick1" counts the number of times the code of this task has
       * been executed. The resolution of this integer timer is 0.01, which is the step size
       * of the task. Size of "clockTick1" ensures timer will not overflow during the
       * application lifespan selected.
       */
      Autotrans_shift_M->Timing.clockTick1++;
    }

    rate_scheduler();
  }                                    /* end MajorTimeStep */
}

/* Derivatives for root system: '<Root>' */
void Autotrans_shift_derivatives(void)
{
  boolean_T lsat;
  boolean_T usat;
  XDot_Autotrans_shift_T *_rtXdot;
  _rtXdot = ((XDot_Autotrans_shift_T *) Autotrans_shift_M->derivs);

  /* Derivatives for Integrator: '<S5>/Wheel Speed' */
  _rtXdot->WheelSpeed_CSTATE = Autotrans_shift_B.VehicleInertia;

  /* Derivatives for Integrator: '<S1>/Integrator' */
  lsat = (Autotrans_shift_X.Integrator_CSTATE <= 600.0);
  usat = (Autotrans_shift_X.Integrator_CSTATE >= 6000.0);
  if (((!lsat) && (!usat)) || (lsat && (Autotrans_shift_B.engineimpellerinertia >
        0.0)) || (usat && (Autotrans_shift_B.engineimpellerinertia < 0.0))) {
    _rtXdot->Integrator_CSTATE = Autotrans_shift_B.engineimpellerinertia;
  } else {
    /* in saturation */
    _rtXdot->Integrator_CSTATE = 0.0;
  }

  /* End of Derivatives for Integrator: '<S1>/Integrator' */
}

/* Model initialize function */
void Autotrans_shift_initialize(void)
{
  /* Registration code */

  /* initialize non-finites */
  rt_InitInfAndNaN(sizeof(real_T));

  /* initialize real-time model */
  (void) memset((void *)Autotrans_shift_M, 0,
                sizeof(RT_MODEL_Autotrans_shift_T));

  {
    /* Setup solver object */
    rtsiSetSimTimeStepPtr(&Autotrans_shift_M->solverInfo,
                          &Autotrans_shift_M->Timing.simTimeStep);
    rtsiSetTPtr(&Autotrans_shift_M->solverInfo, &rtmGetTPtr(Autotrans_shift_M));
    rtsiSetStepSizePtr(&Autotrans_shift_M->solverInfo,
                       &Autotrans_shift_M->Timing.stepSize0);
    rtsiSetdXPtr(&Autotrans_shift_M->solverInfo, &Autotrans_shift_M->derivs);
    rtsiSetContStatesPtr(&Autotrans_shift_M->solverInfo, (real_T **)
                         &Autotrans_shift_M->contStates);
    rtsiSetNumContStatesPtr(&Autotrans_shift_M->solverInfo,
      &Autotrans_shift_M->Sizes.numContStates);
    rtsiSetNumPeriodicContStatesPtr(&Autotrans_shift_M->solverInfo,
      &Autotrans_shift_M->Sizes.numPeriodicContStates);
    rtsiSetPeriodicContStateIndicesPtr(&Autotrans_shift_M->solverInfo,
      &Autotrans_shift_M->periodicContStateIndices);
    rtsiSetPeriodicContStateRangesPtr(&Autotrans_shift_M->solverInfo,
      &Autotrans_shift_M->periodicContStateRanges);
    rtsiSetErrorStatusPtr(&Autotrans_shift_M->solverInfo, (&rtmGetErrorStatus
      (Autotrans_shift_M)));
    rtsiSetRTModelPtr(&Autotrans_shift_M->solverInfo, Autotrans_shift_M);
  }

  rtsiSetSimTimeStep(&Autotrans_shift_M->solverInfo, MAJOR_TIME_STEP);
  Autotrans_shift_M->intgData.y = Autotrans_shift_M->odeY;
  Autotrans_shift_M->intgData.f[0] = Autotrans_shift_M->odeF[0];
  Autotrans_shift_M->intgData.f[1] = Autotrans_shift_M->odeF[1];
  Autotrans_shift_M->intgData.f[2] = Autotrans_shift_M->odeF[2];
  Autotrans_shift_M->intgData.f[3] = Autotrans_shift_M->odeF[3];
  Autotrans_shift_M->intgData.f[4] = Autotrans_shift_M->odeF[4];
  Autotrans_shift_M->intgData.f[5] = Autotrans_shift_M->odeF[5];
  Autotrans_shift_M->contStates = ((X_Autotrans_shift_T *) &Autotrans_shift_X);
  rtsiSetSolverData(&Autotrans_shift_M->solverInfo, (void *)
                    &Autotrans_shift_M->intgData);
  rtsiSetSolverName(&Autotrans_shift_M->solverInfo,"ode5");
  rtmSetTPtr(Autotrans_shift_M, &Autotrans_shift_M->Timing.tArray[0]);
  Autotrans_shift_M->Timing.stepSize0 = 0.01;

  /* block I/O */
  {
    Autotrans_shift_B.VehicleSpeed = 0.0;
    Autotrans_shift_B.Integrator = 0.0;
    Autotrans_shift_B.LookUpTable = 0.0;
    Autotrans_shift_B.TransmissionRPM = 0.0;
    Autotrans_shift_B.engineimpellerinertia = 0.0;
    Autotrans_shift_B.OutputTorque = 0.0;
    Autotrans_shift_B.VehicleInertia = 0.0;
    Autotrans_shift_B.gear = 0.0;
    Autotrans_shift_B.interp_down = 0.0;
    Autotrans_shift_B.interp_up = 0.0;
  }

  /* states (continuous) */
  {
    (void) memset((void *)&Autotrans_shift_X, 0,
                  sizeof(X_Autotrans_shift_T));
  }

  /* states (dwork) */
  (void) memset((void *)&Autotrans_shift_DW, 0,
                sizeof(DW_Autotrans_shift_T));

  /* external inputs */
  Autotrans_shift_U.throttle = 0.0;
  Autotrans_shift_U.brake = 0.0;

  /* external outputs */
  Autotrans_shift_Y.speed = 0.0;
  Autotrans_shift_Y.RPM = 0.0;
  Autotrans_shift_Y.gear = 0.0;

  /* InitializeConditions for Integrator: '<S5>/Wheel Speed' */
  Autotrans_shift_X.WheelSpeed_CSTATE = 0.0;

  /* InitializeConditions for Integrator: '<S1>/Integrator' */
  Autotrans_shift_X.Integrator_CSTATE = 1000.0;

  /* SystemInitialize for Chart: '<Root>/ShiftLogic' */
  Autotrans_shift_DW.is_active_gear_state = 0U;
  Autotrans_shift_DW.is_gear_state = Autotrans_sh_IN_NO_ACTIVE_CHILD;
  Autotrans_shift_DW.is_active_selection_state = 0U;
  Autotrans_shift_DW.is_selection_state = Autotrans_sh_IN_NO_ACTIVE_CHILD;
  Autotrans_shift_DW.temporalCounter_i1 = 0U;
  Autotrans_shift_DW.is_active_c1_Autotrans_shift = 0U;
  Autotrans_shift_B.gear = 0.0;
}

/* Model terminate function */
void Autotrans_shift_terminate(void)
{
  /* (no terminate code required) */
}

/*
 * File trailer for generated code.
 *
 * [EOF]
 */
