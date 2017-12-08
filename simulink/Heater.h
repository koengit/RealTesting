/*
 * Academic License - for use in teaching, academic research, and meeting
 * course requirements at degree granting institutions only.  Not for
 * government, commercial, or other organizational use.
 *
 * File: Heater.h
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

#ifndef RTW_HEADER_Heater_h_
#define RTW_HEADER_Heater_h_
#include <string.h>
#ifndef Heater_COMMON_INCLUDES_
# define Heater_COMMON_INCLUDES_
#include "rtwtypes.h"
#include "rtw_continuous.h"
#include "rtw_solver.h"
#endif                                 /* Heater_COMMON_INCLUDES_ */

#include "Heater_types.h"

/* Macros for accessing real-time model data structure */
#ifndef rtmGetErrorStatus
# define rtmGetErrorStatus(rtm)        ((rtm)->errorStatus)
#endif

#ifndef rtmSetErrorStatus
# define rtmSetErrorStatus(rtm, val)   ((rtm)->errorStatus = (val))
#endif

#ifndef rtmGetStopRequested
# define rtmGetStopRequested(rtm)      ((rtm)->Timing.stopRequestedFlag)
#endif

#ifndef rtmSetStopRequested
# define rtmSetStopRequested(rtm, val) ((rtm)->Timing.stopRequestedFlag = (val))
#endif

#ifndef rtmGetStopRequestedPtr
# define rtmGetStopRequestedPtr(rtm)   (&((rtm)->Timing.stopRequestedFlag))
#endif

#ifndef rtmGetT
# define rtmGetT(rtm)                  (rtmGetTPtr((rtm))[0])
#endif

/* Block signals (auto storage) */
typedef struct {
  real_T FilterCoefficient;            /* '<S3>/Filter Coefficient' */
  real_T Divide;                       /* '<S1>/Divide' */
  real_T Divide1;                      /* '<S1>/Divide1' */
  real_T IntegralGain;                 /* '<S3>/Integral Gain' */
} B_Heater_T;

/* Continuous states (auto storage) */
typedef struct {
  real_T rIntegrator_CSTATE;           /* '<S1>/r Integrator' */
  real_T hIntegrator_CSTATE;           /* '<S1>/h Integrator' */
  real_T Integrator_CSTATE;            /* '<S3>/Integrator' */
  real_T Filter_CSTATE;                /* '<S3>/Filter' */
} X_Heater_T;

/* State derivatives (auto storage) */
typedef struct {
  real_T rIntegrator_CSTATE;           /* '<S1>/r Integrator' */
  real_T hIntegrator_CSTATE;           /* '<S1>/h Integrator' */
  real_T Integrator_CSTATE;            /* '<S3>/Integrator' */
  real_T Filter_CSTATE;                /* '<S3>/Filter' */
} XDot_Heater_T;

/* State disabled  */
typedef struct {
  boolean_T rIntegrator_CSTATE;        /* '<S1>/r Integrator' */
  boolean_T hIntegrator_CSTATE;        /* '<S1>/h Integrator' */
  boolean_T Integrator_CSTATE;         /* '<S3>/Integrator' */
  boolean_T Filter_CSTATE;             /* '<S3>/Filter' */
} XDis_Heater_T;

/* Invariant block signals (auto storage) */
typedef struct {
  const real_T Sum2;                   /* '<S1>/Sum2' */
  const real_T OCOT;                   /* '<S1>/OC*OT' */
  const real_T Sum4;                   /* '<S1>/Sum4' */
  const real_T Sum5;                   /* '<S1>/Sum5' */
} ConstB_Heater_T;

#ifndef ODE3_INTG
#define ODE3_INTG

/* ODE3 Integration Data */
typedef struct {
  real_T *y;                           /* output */
  real_T *f[3];                        /* derivatives */
} ODE3_IntgData;

#endif

/* External inputs (root inport signals with auto storage) */
typedef struct {
  real_T goaltemperature;              /* '<Root>/g' */
} ExtU_Heater_T;

/* External outputs (root outports fed by signals with auto storage) */
typedef struct {
  real_T r;                            /* '<Root>/r' */
  real_T h;                            /* '<Root>/h' */
  real_T l;                            /* '<Root>/l' */
} ExtY_Heater_T;

/* Real-time Model Data Structure */
struct tag_RTM_Heater_T {
  const char_T *errorStatus;
  RTWSolverInfo solverInfo;
  X_Heater_T *contStates;
  int_T *periodicContStateIndices;
  real_T *periodicContStateRanges;
  real_T *derivs;
  boolean_T *contStateDisabled;
  boolean_T zCCacheNeedsReset;
  boolean_T derivCacheNeedsReset;
  boolean_T blkStateChange;
  real_T odeY[4];
  real_T odeF[3][4];
  ODE3_IntgData intgData;

  /*
   * Sizes:
   * The following substructure contains sizes information
   * for many of the model attributes such as inputs, outputs,
   * dwork, sample times, etc.
   */
  struct {
    int_T numContStates;
    int_T numPeriodicContStates;
    int_T numSampTimes;
  } Sizes;

  /*
   * Timing:
   * The following substructure contains information regarding
   * the timing information for the model.
   */
  struct {
    uint32_T clockTick0;
    time_T stepSize0;
    uint32_T clockTick1;
    SimTimeStep simTimeStep;
    boolean_T stopRequestedFlag;
    time_T *t;
    time_T tArray[2];
  } Timing;
};

/* Block signals (auto storage) */
extern B_Heater_T Heater_B;

/* Continuous states (auto storage) */
extern X_Heater_T Heater_X;

/* External inputs (root inport signals with auto storage) */
extern ExtU_Heater_T Heater_U;

/* External outputs (root outports fed by signals with auto storage) */
extern ExtY_Heater_T Heater_Y;
extern const ConstB_Heater_T Heater_ConstB;/* constant block i/o */

/* Model entry point functions */
extern void Heater_initialize(void);
extern void Heater_step(void);
extern void Heater_terminate(void);

/* Real-time Model object */
extern RT_MODEL_Heater_T *const Heater_M;

/*-
 * These blocks were eliminated from the model due to optimizations:
 *
 * Block '<Root>/Scope' : Unused code path elimination
 * Block '<Root>/Manual Switch' : Eliminated due to constant selection input
 * Block '<Root>/Constant' : Unused code path elimination
 */

/*-
 * The generated code includes comments that allow you to trace directly
 * back to the appropriate location in the model.  The basic format
 * is <system>/block_name, where system is the system number (uniquely
 * assigned by Simulink) and block_name is the name of the block.
 *
 * Use the MATLAB hilite_system command to trace the generated code back
 * to the model.  For example,
 *
 * hilite_system('<S3>')    - opens system 3
 * hilite_system('<S3>/Kp') - opens and selects block Kp which resides in S3
 *
 * Here is the system hierarchy for this model
 *
 * '<Root>' : 'Heater'
 * '<S1>'   : 'Heater/Continuous Plant'
 * '<S2>'   : 'Heater/Controller'
 * '<S3>'   : 'Heater/Controller/Broken controller'
 */
#endif                                 /* RTW_HEADER_Heater_h_ */

/*
 * File trailer for generated code.
 *
 * [EOF]
 */
