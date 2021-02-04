using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class LeverScript : MonoBehaviour, IInteractable
{
    public Animator animator;
    void Start() 
    {
        gameObject.layer = LayerMask.NameToLayer("Interactable");

    }
    public void Interact()
    {
        animator.SetTrigger("Move");
    }

}
