using System;
using System.Collections;
using System.Collections.Generic;
using Scary_event_System.Custom_Event_System;
using Scary_event_System.FMOD_Sound_Event;
using UnityEngine;

public class SoundManager : MonoBehaviour
{
    [FMODUnity.EventRef]
    public string musicEvent = "event:/Music/Music";

    [FMODUnity.EventRef]
    public string ambienceEvent = "event:/Ambience/Ambience";

    //public string environment = "event:/Environmental";
    
   // public static FMOD.Studio.EventInstance puzzleInstance;

    
    private static FMOD.Studio.EventInstance musicInstance;
    private static FMOD.Studio.EventInstance ambienceInstance;

    private FMOD.Studio.PARAMETER_ID presenceParameterID;
    private FMOD.Studio.PARAMETER_ID ambienceParameterID;

    private float currentPresenceValue;
    private float targetPresenceValue;

    private float currentAmbienceValue;
    private float targetAmbienceValue;

    private float t;

    private bool _isRegister = false;




    private void OnValidate()
    {
        if (!_isRegister)
        {
            CustomEventManager.RegisterListener<EventInfo.FmodEventInfo>(ChangePreset);
        }
        _isRegister = true;
    }

    private void ChangePreset(EventInfo eventinfo)
    {
        var thisEvent = (EventInfo.FmodEventInfo)eventinfo;
        float targetPre = thisEvent.TargetPresenceValue;
        float targetAmb = thisEvent.TargetAmbienceValue;

        if (targetPresenceValue != targetPre && targetPre!= -1) targetPresenceValue = targetPre;
        if (targetAmbienceValue != targetAmb && targetAmb!= -1) targetAmbienceValue = targetAmb;
    }

    private void OnDisable()
    {
        CustomEventManager.UnregisterListener<EventInfo.FmodEventInfo>(ChangePreset);
    }

    private void Awake()
    
    {
        CustomEventManager.RegisterListener<EventInfo.FmodEventInfo>(ChangePreset);
        musicInstance = FMODUnity.RuntimeManager.CreateInstance(musicEvent);
        ambienceInstance = FMODUnity.RuntimeManager.CreateInstance(ambienceEvent);
       // globalInstance = FMODUnity.RuntimeManager.CreateInstance(environment);

        //Change speed
        t = 0.0001f;
    }

    private void Start()
    {
        currentPresenceValue = 0f;
        currentAmbienceValue = 0f;
        musicInstance.start();
        ambienceInstance.start();
    }

    private void Update()
    {
        currentPresenceValue = Mathf.Lerp(currentPresenceValue, targetPresenceValue, t / (Mathf.Abs(targetPresenceValue-currentPresenceValue)));
        currentAmbienceValue = Mathf.Lerp(currentAmbienceValue, targetAmbienceValue, t / (Mathf.Abs(targetAmbienceValue - currentAmbienceValue))*10);

        FMODUnity.RuntimeManager.StudioSystem.setParameterByName("Presence", currentPresenceValue);
        FMODUnity.RuntimeManager.StudioSystem.setParameterByName("Ambience", currentAmbienceValue);
    }
}
