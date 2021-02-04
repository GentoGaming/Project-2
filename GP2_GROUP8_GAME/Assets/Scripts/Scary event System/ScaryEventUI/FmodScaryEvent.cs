using System.Threading.Tasks;
using Scary_event_System.Custom_Event_System;
using Scary_event_System.FMOD_Sound_Event;
using UnityEngine;

namespace Scary_event_System.ScaryEventUI
{
    public class FmodScaryEvent : ScaryEventAbstract
    {
        [Header("--------------------------------------------------------------------------------------")]
        public float targetPresenceValue;

        public float targetAmbienceValue;


        public override async Task ConditionExecute()
        {
            await Task.Delay((int) (delayEvent * 1000));
            EventInfo.FmodEventInfo fmoDei = new EventInfo.FmodEventInfo(targetPresenceValue, targetAmbienceValue);
            CustomEventManager.SendNewEvent(fmoDei);   
        }

        public override async Task CustomUpdate()
        {
            // Custom Async Not implemented for FMOD library
        }
    }
}