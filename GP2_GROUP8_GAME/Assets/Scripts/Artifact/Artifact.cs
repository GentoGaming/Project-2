using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Artifact : MonoBehaviour, IInteractable
{
    public enum Aspect {Physics, Chemistry, Knowledge };
    public Aspect aspect;

    //FMOD stuff
    [FMODUnity.EventRef]
    public string ArtifactGet = "event:/Music/ArtifactGet";
    [FMODUnity.EventRef]
    public string PickUp = "event:/Player/PickUp";
    public void Interact()
    {
       
        switch (aspect)
        {
            case Aspect.Physics:
                GameManager.instance.inventory.EngineeringArtefact = true;
                break;
            case Aspect.Knowledge:
                GameManager.instance.inventory.LibraryArtefact = true;
                break;
            case Aspect.Chemistry:
                GameManager.instance.inventory.ChemistryArtefact = true;
                break;
            default:
                    break;
                
        }
        //Play sound
        FMODUnity.RuntimeManager.PlayOneShot(ArtifactGet);
        FMODUnity.RuntimeManager.PlayOneShot(PickUp);
        Destroy(this.gameObject);

        
    }
}
