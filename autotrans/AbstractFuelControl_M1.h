/*
 * Academic License - for use in teaching, academic research, and meeting
 * course requirements at degree granting institutions only.  Not for
 * government, commercial, or other organizational use.
 *
 * File: AbstractFuelControl_M1.h
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

#ifndef RTW_HEADER_AbstractFuelControl_M1_h_
#define RTW_HEADER_AbstractFuelControl_M1_h_
#include <math.h>
#include <string.h>
#ifndef AbstractFuelControl_M1_COMMON_INCLUDES_
# define AbstractFuelControl_M1_COMMON_INCLUDES_
#include "rtwtypes.h"
#include "zero_crossing_types.h"
#include "rtw_continuous.h"
#include "rtw_solver.h"
#endif                                 /* AbstractFuelControl_M1_COMMON_INCLUDES_ */

#include "AbstractFuelControl_M1_types.h"
#include "rt_zcfcn.h"
#include "rt_nonfinite.h"
#include "rtGetInf.h"

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
  real_T Gain;                         /* '<S19>/Gain' */
  real_T fuelinjectortolerance095105;  /* '<S1>/fuel injector tolerance [0.95 1.05]' */
  real_T airbyfuel;                    /* '<S3>/Divide' */
  real_T Gain1;                        /* '<S18>/Gain1' */
  real_T delays;                       /* '<S3>/delay (s)' */
  real_T RTVm;                         /* '<S4>/RT//Vm' */
  real_T Sum;                          /* '<S6>/Sum' */
  real_T Pwon;                         /* '<S2>/Pwon' */
  real32_T Sum3;                       /* '<S12>/Sum3' */
} B_AbstractFuelControl_M1_T;

/* Block states (auto storage) for system '<Root>' */
typedef struct {
  struct {
    real_T modelTStart;
    real_T TUbufferArea[61440];
  } fuelsystemtransportdelay_RWORK;    /* '<S3>/fuel system transport delay' */

  struct {
    void *TUbufferPtrs[3];
  } fuelsystemtransportdelay_PWORK;    /* '<S3>/fuel system transport delay' */

  real32_T UnitDelay2_DSTATE;          /* '<S14>/Unit Delay2' */
  real32_T UnitDelay1_DSTATE;          /* '<S12>/UnitDelay1' */
  real32_T UnitDelay1_DSTATE_d;        /* '<S11>/UnitDelay1' */
  real32_T commanded_fuel;             /* '<S2>/commanded_fuel' */
  real32_T airbyfuel_ref;              /* '<S2>/mode_fb1' */
  real32_T engine_speed;               /* '<S7>/DataStoreMemory' */
  real32_T throttle_flow;              /* '<S7>/DataStoreMemory1' */
  real32_T airbyfuel_meas;             /* '<S7>/DataStoreMemory2' */
  real32_T throttle_angle;             /* '<S7>/DataStoreMemory3' */
  int32_T clockTickCounter;            /* '<S2>/PulseGenerator_10ms' */
  struct {
    int_T Tail;
    int_T Head;
    int_T Last;
    int_T CircularBufSize;
  } fuelsystemtransportdelay_IWORK;    /* '<S3>/fuel system transport delay' */

  boolean_T UnitDelay_DSTATE;          /* '<S16>/Unit Delay' */
  boolean_T UnitDelay1_DSTATE_a;       /* '<S15>/Unit Delay1' */
  boolean_T UnitDelay1_DSTATE_e;       /* '<S14>/Unit Delay1' */
  boolean_T controller_mode;           /* '<S2>/mode_fb' */
} DW_AbstractFuelControl_M1_T;

/* Continuous states (auto storage) */
typedef struct {
  real_T Integrator_CSTATE;            /* '<S19>/Integrator' */
  real_T Throttledelay_CSTATE;         /* '<S1>/Throttle delay' */
  real_T p00543bar_CSTATE;             /* '<S4>/p0 = 0.543 (bar)' */
  real_T Integrator_CSTATE_h;          /* '<S18>/Integrator' */
  real_T Integrator_CSTATE_c;          /* '<S6>/Integrator' */
  real_T fuelsystemtransportdelay_CSTATE;/* '<S3>/fuel system transport delay' */
} X_AbstractFuelControl_M1_T;

/* State derivatives (auto storage) */
typedef struct {
  real_T Integrator_CSTATE;            /* '<S19>/Integrator' */
  real_T Throttledelay_CSTATE;         /* '<S1>/Throttle delay' */
  real_T p00543bar_CSTATE;             /* '<S4>/p0 = 0.543 (bar)' */
  real_T Integrator_CSTATE_h;          /* '<S18>/Integrator' */
  real_T Integrator_CSTATE_c;          /* '<S6>/Integrator' */
  real_T fuelsystemtransportdelay_CSTATE;/* '<S3>/fuel system transport delay' */
} XDot_AbstractFuelControl_M1_T;

/* State disabled  */
typedef struct {
  boolean_T Integrator_CSTATE;         /* '<S19>/Integrator' */
  boolean_T Throttledelay_CSTATE;      /* '<S1>/Throttle delay' */
  boolean_T p00543bar_CSTATE;          /* '<S4>/p0 = 0.543 (bar)' */
  boolean_T Integrator_CSTATE_h;       /* '<S18>/Integrator' */
  boolean_T Integrator_CSTATE_c;       /* '<S6>/Integrator' */
  boolean_T fuelsystemtransportdelay_CSTATE;/* '<S3>/fuel system transport delay' */
} XDis_AbstractFuelControl_M1_T;

/* Zero-crossing (trigger) state */
typedef struct {
  ZCSigState fuel_controller_pwon_Trig_ZCE;/* '<S7>/fuel_controller_pwon' */
  ZCSigState fuel_controller_mode_10ms_Trig_;/* '<S7>/fuel_controller_mode_10ms' */
  ZCSigState fuel_controller_10ms_Trig_ZCE;/* '<S7>/fuel_controller_10ms' */
} PrevZCX_AbstractFuelControl_M_T;

/* Invariant block signals (auto storage) */
typedef struct {
  const real32_T Sum;                  /* '<S15>/Sum' */
} ConstB_AbstractFuelControl_M1_T;

#ifndef ODE3_INTG
#define ODE3_INTG

/* ODE3 Integration Data */
typedef struct {
  real_T *y;                           /* output */
  real_T *f[3];                        /* derivatives */
} ODE3_IntgData;

#endif

/* Constant parameters (auto storage) */
typedef struct {
  /* Expression: reshape([0.8,.7,.7,.8,.9,.7,.66,.65,.73,.85,.66,.66,.63,.66,.8,.6,.6,.6,.6,.7],5,4)
   * Referenced by: '<S6>/1-Kappa'
   */
  real_T uKappa_tableData[20];

  /* Pooled Parameter (Expression: [1000,1500,2000,2500,3000])
   * Referenced by:
   *   '<S6>/1-Kappa'
   *   '<S6>/tau_ww'
   */
  real_T pooled9[5];

  /* Pooled Parameter (Mixed Expressions)
   * Referenced by:
   *   '<S6>/1-Kappa'
   *   '<S6>/tau_ww'
   */
  real_T pooled10[4];

  /* Expression: reshape([.4,.3,.35,.3,.2,.22,.22,.4,.35,.5,.20,.22,.5,.4,.35,.35,.3,.45,.5,.4],5,4)
   * Referenced by: '<S6>/tau_ww'
   */
  real_T tau_ww_tableData[20];

  /* Expression: reshape([0.8,0.6,0.4,0.3,0.2,0.4,0.3,0.2,0.2,0.2,0.3,0.25,0.2,0.2,0.2,0.25,0.2,0.2,0.2,0.2],5,4)
   * Referenced by: '<S3>/delay (s)'
   */
  real_T delays_tableData[20];

  /* Expression: [800,1000,1500,2000,3000]
   * Referenced by: '<S3>/delay (s)'
   */
  real_T delays_bp01Data[5];

  /* Expression: [0.05,0.15,0.2,0.25]
   * Referenced by: '<S3>/delay (s)'
   */
  real_T delays_bp02Data[4];

  /* Pooled Parameter (Expression: )
   * Referenced by:
   *   '<S3>/delay (s)'
   *   '<S6>/1-Kappa'
   *   '<S6>/tau_ww'
   */
  uint32_T pooled16[2];
} ConstP_AbstractFuelControl_M1_T;

/* External inputs (root inport signals with auto storage) */
typedef struct {
  real_T PedalAngle;                   /* '<Root>/Pedal Angle' */
  real_T EngineSpeed;                  /* '<Root>/Engine Speed' */
} ExtU_AbstractFuelControl_M1_T;

/* External outputs (root outports fed by signals with auto storage) */
typedef struct {
  real_T AFref;                        /* '<Root>/AFref' */
  real_T AF;                           /* '<Root>/AF' */
  real_T controller_mode;              /* '<Root>/controller_mode' */
} ExtY_AbstractFuelControl_M1_T;

/* Real-time Model Data Structure */
struct tag_RTM_AbstractFuelControl_M_T {
  const char_T *errorStatus;
  RTWSolverInfo solverInfo;
  X_AbstractFuelControl_M1_T *contStates;
  int_T *periodicContStateIndices;
  real_T *periodicContStateRanges;
  real_T *derivs;
  boolean_T *contStateDisabled;
  boolean_T zCCacheNeedsReset;
  boolean_T derivCacheNeedsReset;
  boolean_T blkStateChange;
  real_T odeY[6];
  real_T odeF[3][6];
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
    struct {
      uint8_T TID[3];
    } TaskCounters;

    SimTimeStep simTimeStep;
    boolean_T stopRequestedFlag;
    time_T *t;
    time_T tArray[3];
  } Timing;
};

/* Block signals (auto storage) */
extern B_AbstractFuelControl_M1_T AbstractFuelControl_M1_B;

/* Continuous states (auto storage) */
extern X_AbstractFuelControl_M1_T AbstractFuelControl_M1_X;

/* Block states (auto storage) */
extern DW_AbstractFuelControl_M1_T AbstractFuelControl_M1_DW;

/* External inputs (root inport signals with auto storage) */
extern ExtU_AbstractFuelControl_M1_T AbstractFuelControl_M1_U;

/* External outputs (root outports fed by signals with auto storage) */
extern ExtY_AbstractFuelControl_M1_T AbstractFuelControl_M1_Y;
extern const ConstB_AbstractFuelControl_M1_T AbstractFuelControl_M1_ConstB;/* constant block i/o */

/* Constant parameters (auto storage) */
extern const ConstP_AbstractFuelControl_M1_T AbstractFuelControl_M1_ConstP;

/* Model entry point functions */
extern void AbstractFuelControl_M1_initialize(void);
extern void AbstractFuelControl_M1_step(void);
extern void AbstractFuelControl_M1_terminate(void);

/* Real-time Model object */
extern RT_MODEL_AbstractFuelControl__T *const AbstractFuelControl_M1_M;

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
 * '<Root>' : 'AbstractFuelControl_M1'
 * '<S1>'   : 'AbstractFuelControl_M1/Model 1'
 * '<S2>'   : 'AbstractFuelControl_M1/Model 1/AF_Controller'
 * '<S3>'   : 'AbstractFuelControl_M1/Model 1/Cylinder and Exhaust'
 * '<S4>'   : 'AbstractFuelControl_M1/Model 1/Intake Manifold'
 * '<S5>'   : 'AbstractFuelControl_M1/Model 1/Throttle'
 * '<S6>'   : 'AbstractFuelControl_M1/Model 1/Wall wetting'
 * '<S7>'   : 'AbstractFuelControl_M1/Model 1/AF_Controller/fuel_controller'
 * '<S8>'   : 'AbstractFuelControl_M1/Model 1/AF_Controller/fuel_controller/fuel_controller_10ms'
 * '<S9>'   : 'AbstractFuelControl_M1/Model 1/AF_Controller/fuel_controller/fuel_controller_mode_10ms'
 * '<S10>'  : 'AbstractFuelControl_M1/Model 1/AF_Controller/fuel_controller/fuel_controller_pwon'
 * '<S11>'  : 'AbstractFuelControl_M1/Model 1/AF_Controller/fuel_controller/fuel_controller_10ms/air_estimation'
 * '<S12>'  : 'AbstractFuelControl_M1/Model 1/AF_Controller/fuel_controller/fuel_controller_10ms/feedback_PI_controller'
 * '<S13>'  : 'AbstractFuelControl_M1/Model 1/AF_Controller/fuel_controller/fuel_controller_10ms/feedforward_controller'
 * '<S14>'  : 'AbstractFuelControl_M1/Model 1/AF_Controller/fuel_controller/fuel_controller_mode_10ms/normal_mode_detection'
 * '<S15>'  : 'AbstractFuelControl_M1/Model 1/AF_Controller/fuel_controller/fuel_controller_mode_10ms/power_mode_detection'
 * '<S16>'  : 'AbstractFuelControl_M1/Model 1/AF_Controller/fuel_controller/fuel_controller_mode_10ms/sensor_failure_detection'
 * '<S17>'  : 'AbstractFuelControl_M1/Model 1/Cylinder and Exhaust/A//F_sensor'
 * '<S18>'  : 'AbstractFuelControl_M1/Model 1/Cylinder and Exhaust/Filter'
 * '<S19>'  : 'AbstractFuelControl_M1/Model 1/Cylinder and Exhaust/A//F_sensor/Filter'
 */
#endif                                 /* RTW_HEADER_AbstractFuelControl_M1_h_ */

/*
 * File trailer for generated code.
 *
 * [EOF]
 */
