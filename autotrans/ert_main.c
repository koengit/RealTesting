/*
 * Academic License - for use in teaching, academic research, and meeting
 * course requirements at degree granting institutions only.  Not for
 * government, commercial, or other organizational use.
 *
 * File: ert_main.c
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

#include <stddef.h>
#include <stdio.h>                     /* This ert_main.c example uses printf/fflush */
#include "Autotrans_shift.h"           /* Model's header file */
#include "rtwtypes.h"

/*
 * Associating rt_OneStep with a real-time clock or interrupt service routine
 * is what makes the generated code "real-time".  The function rt_OneStep is
 * always associated with the base rate of the model.  Subrates are managed
 * by the base rate from inside the generated code.  Enabling/disabling
 * interrupts and floating point context switches are target specific.  This
 * example code indicates where these should take place relative to executing
 * the generated code step function.  Overrun behavior should be tailored to
 * your application needs.  This example simply sets an error status in the
 * real-time model and returns from rt_OneStep.
 */
void rt_OneStep(void);
void rt_OneStep(void)
{
  static boolean_T OverrunFlag = false;

  /* Disable interrupts here */

  /* Check for overrun */
  if (OverrunFlag) {
    rtmSetErrorStatus(Autotrans_shift_M, "Overrun");
    return;
  }

  OverrunFlag = true;

  /* Save FPU context here (if necessary) */
  /* Re-enable timer or interrupt here */
  /* Set model inputs here */

  /* Step the model for base rate */
  Autotrans_shift_step();

  /* Get model outputs here */

  /* Indicate task complete */
  OverrunFlag = false;

  /* Disable interrupts here */
  /* Restore FPU context here (if necessary) */
  /* Enable interrupts here */
}

/*
 * The example "main" function illustrates what is required by your
 * application code to initialize, execute, and terminate the generated code.
 * Attaching rt_OneStep to a real-time clock is target specific.  This example
 * illustrates how you do this relative to initializing the model.
 */
int_T main(int_T argc, const char *argv[])
{
  /* Unused arguments */
  (void)(argc);
  (void)(argv);

  /* Initialize model */
  Autotrans_shift_initialize();
  Autotrans_shift_U.throttle=52;
  Autotrans_shift_U.brake=0;

  /* Simulating the model step behavior (in non real-time) to
   *  simulate model behavior at stop time.
   */
  while ((rtmGetErrorStatus(Autotrans_shift_M) == (NULL)) &&
         !rtmGetStopRequested(Autotrans_shift_M)) {
    if (Autotrans_shift_M->Timing.clockTick0 % 100 == 0)
        printf("time=%lf speed=%lf rpm=%lf gear=%lf throttle=%lf brake=%lf\n", Autotrans_shift_M->Timing.clockTick0 * Autotrans_shift_M->Timing.stepSize0, Autotrans_shift_Y.speed, Autotrans_shift_Y.RPM, Autotrans_shift_Y.gear, Autotrans_shift_U.throttle, Autotrans_shift_U.brake);
    rt_OneStep();

    switch(Autotrans_shift_M->Timing.clockTick0) {
    case 500:
        Autotrans_shift_U.throttle=90;
        break;
    case 1000:
        Autotrans_shift_U.throttle=60;
        break;
    case 1500:
        Autotrans_shift_U.throttle=85;
        break;
    case 2000:
        Autotrans_shift_U.throttle=58;
        break;
    case 2500:
        Autotrans_shift_U.throttle=65;
        break;
    case 3001:
        return 0;
    }
  }

  /* Disable rt_OneStep() here */

  /* Terminate model */
  Autotrans_shift_terminate();
  return 0;
}

/*
 * File trailer for generated code.
 *
 * [EOF]
 */
