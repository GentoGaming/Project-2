using System;
using System.Collections;
using System.Collections.Generic;
using System.Net.Configuration;
using Electric_Puzzle;
using FMOD.Studio;
using Scary_event_System.Custom_Event_System;
using UnityEngine;
[RequireComponent(typeof(Rigidbody))]
public class PressingButtonInteaction : MonoBehaviour, IInteractable
{

    public Animator animator;
    private bool puzzleOver;
    [HideInInspector] public bool buttonState = false;
    [SerializeField] private float timeBeforeTurnOff = 1;
    [SerializeField] private ParticleSystem[] particleSystemList;
    private GameObject _electricNodeParent;
    private float _minDistance;
    private List<Transform> _allNodes = new List<Transform>();
    private Transform _rootNode;
    public ParticleSystem sparkling;
    private bool _isNotSolved = true;
    public float electricityOffDelay = 5f;
    [FMODUnity.EventRef] public string electricityRunning = "event:/Environmental/electricityRunning";
    [FMODUnity.EventRef] public string Lever = "event:/Environmental/Lever";
    [FMODUnity.EventRef] public string electricityStream = "event:/Environmental/electricityStream";
    [FMODUnity.EventRef] public string Success = "event:/Music/Success";
    FMOD.Studio.EventInstance _leverInstance, _generatorInstance, _eleStreamInstance;
    



    private void OnEnable()
    {
        EventManager.StartListening("DisableEleButton", TurnOffButton);

    }

    private void OnDisable()
    {
        EventManager.StopListening("DisableEleButton", TurnOffButton);
    }


    private void Awake()
    {
        _electricNodeParent = GameObject.Find("ElectricNodes");
        _rootNode = GameObject.Find("RootNode").transform;
        Electric ele = _electricNodeParent.GetComponent<Electric>();
        if (ele != null)
        {
            _minDistance = ele.powerRange;
        }

        foreach (Transform child in _electricNodeParent.transform)
        {
            if (child.gameObject == _rootNode.gameObject) continue;
            _allNodes.Add(child);
        }
    }


    private IEnumerator CustomUpdate()
    {
        while (true)
        {
            yield return new WaitForSeconds(timeBeforeTurnOff);
            if (buttonState == true)
            {
                bool foundOneInRange = false;
                foreach (var node in _allNodes)
                {
                    if (Vector3.Distance(_rootNode.position, node.position) < _minDistance)
                    {
                        foundOneInRange = true;
                        break;
                    }
                }

                if (!foundOneInRange)
                {
                    ChangeButtonState();
                    StopCoroutine(CustomUpdate());
                    break;
                }
            }
            else
            {
                break;
            }

        }
    }


    private void TurnOffButton()
    {
        FMODUnity.RuntimeManager.PlayOneShot(Success);
        Invoke(nameof(DelayMethod), electricityOffDelay);

    }

    private void DelayMethod()
    {
        _isNotSolved = false;

        if (buttonState)
        {
            ChangeButtonState();
        }
        puzzleOver = true;
        sparkling.Stop();
        animator.SetTrigger("Light");
        //No longer interactable // Herman
        gameObject.layer = 0;
    }

    public void Interact()
    {

        if (animator.GetCurrentAnimatorStateInfo(0).IsTag("idle"))
        {
            ChangeButtonState();
        }
        


    }

    private void ShutDownElectricityVisual()
    {
        foreach (ParticleSystem ps in particleSystemList)
        {
            ps.Stop();
        }
    }

    private void ChangeButtonState()
    {
        if (puzzleOver) return;
        PlayLeverSound();

        if (buttonState)
        {
            StopGeneratorSound();
            animator.SetTrigger("Move");
            ShutDownElectricityVisual();
            StopCoroutine(CustomUpdate());
        }
        else
        {
            if (_isNotSolved)
            {
                PlayGeneratorSound();
                PlayStreamSound();
                animator.SetTrigger("Move");
                bool isOneParticleActive = false;
                int count = 0;
                foreach (ParticleSystem ps in particleSystemList)
                {
                    ps.Play();
                }

                StartCoroutine(CustomUpdate());
            }
        }

        if (!_isNotSolved)
        {
            buttonState = false;
        }
        else
        {
            buttonState = !buttonState;
        }
    }

    private void StopGeneratorSound()
    {
        _generatorInstance.stop(STOP_MODE.IMMEDIATE);
        _generatorInstance.release();
        _eleStreamInstance.stop(STOP_MODE.IMMEDIATE);
        _eleStreamInstance.release();

    }

    private void PlayStreamSound()
    {
        _eleStreamInstance = FMODUnity.RuntimeManager.CreateInstance(electricityStream); 
        _eleStreamInstance.start();
        FMODUnity.RuntimeManager.AttachInstanceToGameObject( _eleStreamInstance, GetComponent<Transform>(), GetComponent<Rigidbody>()); 
    }

    
    
    private void PlayLeverSound()
    { 
        _leverInstance = FMODUnity.RuntimeManager.CreateInstance(Lever); 
        _leverInstance.start();
       FMODUnity.RuntimeManager.AttachInstanceToGameObject( _leverInstance, GetComponent<Transform>(), GetComponent<Rigidbody>());
    }

    private void PlayGeneratorSound()
    {
        _generatorInstance = FMODUnity.RuntimeManager.CreateInstance(electricityRunning);
        _generatorInstance.start();
        FMODUnity.RuntimeManager.AttachInstanceToGameObject( _generatorInstance, GetComponent<Transform>(), GetComponent<Rigidbody>());
    }
    
    
    
    private void StopLeverSound()
    {
        _leverInstance.stop(STOP_MODE.IMMEDIATE);
        _leverInstance.release();
    }
    
    
}
