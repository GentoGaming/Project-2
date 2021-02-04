using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Scary_event_System.Custom_Event_System;

public class GargoyleButton : MonoBehaviour, IInteractable
{
    public string eventId;
    public Animator thisAnimator;

    //FMOD stuff
    [FMODUnity.EventRef]
    public string Lever = "event:/Environmental/Lever";
    [FMODUnity.EventRef]
    public string Success = "event:/Music/Success";
    public void Interact()
    {
        EventManager.TriggerEvent(eventId);
        thisAnimator.SetTrigger("Play");
        
       

    }

    public void GoPassive()
    {
        //Sets layer to default, so that it is no longer highlighted when player looks at it. In this case, using an int is simpler, because default layer will always be 0, otherwise
        //I would have used a LayerMask. This function is called at the end of the animation, through an animation event. // Herman

        gameObject.layer = 0;
    }

    public void SoundEffects(int sound)
    {
        switch (sound)
        {
            case 1:
                FMODUnity.RuntimeManager.PlayOneShot(Lever, transform.position);
                break;
            case 2:
                FMODUnity.RuntimeManager.PlayOneShot(Success);
                break;
        }
    }

}
