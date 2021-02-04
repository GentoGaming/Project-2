using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Scary_event_System.Custom_Event_System;

public class GeneralButton : MonoBehaviour, IInteractable
{
    public string eventId;
    public void Interact()
    {
        EventManager.TriggerEvent(eventId);
        
    }

}
