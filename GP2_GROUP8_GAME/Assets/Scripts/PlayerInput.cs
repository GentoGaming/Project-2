//@Author: Teodor Tysklind / FutureGames / Teodor.Tysklind@FutureGames.nu

using System;
using Journal_System.Code.Journal_UI;
using UnityEngine;
using UnityEngine.SceneManagement;

public class PlayerInput : MonoBehaviour
{
    private CharacterMovement _characterMovement;
    private CameraController _camera;
    private JournalUI journal;

    [NonSerialized] public bool nullifyInput;

    public bool NullifyInput
    {
        get { return nullifyInput;}
        set
        {
            nullifyInput = value;
            GameManager.instance.ToggleCrosshair(!value);
        }
    }

    public static PlayerInput instance;

    private void Start()
    {
        journal = FindObjectOfType<JournalUI>();
        instance = this;
        _characterMovement = GameManager.instance.player.GetComponent<CharacterMovement>();
        _camera = Camera.main.GetComponentInParent<CameraController>();

        if ("Main_Scene" == SceneManager.GetActiveScene().name)
        {
            nullifyInput = true;
            GameManager.instance.ToggleCrosshair(true);

        }
        else
        {
            nullifyInput = false;
        }
    }

    private void Update()
    {
        if (!nullifyInput)
        {
            SetMovementInput();
            SetCameraInput();
            journal.VisibilityToggle();
        }
        else
        {
            SetZeroInput();
        }
    }

    private void SetMovementInput()
    {
        _characterMovement.xRotInput = Input.GetAxis("Mouse X");
        _characterMovement.strafeMovementInput = Input.GetAxis("Horizontal");
        _characterMovement.forwardMovementInput = Input.GetAxis("Vertical");
    }

    private void SetCameraInput()
    {
        _camera.pitchInput = Input.GetAxis("Mouse Y");
    }

    private void SetZeroInput()
    {
        _characterMovement.xRotInput = 0;
        _characterMovement.strafeMovementInput = 0;
        _characterMovement.forwardMovementInput = 0;
        _camera.pitchInput = 0;

        if (journal._journal.active == true)
        {
            journal.VisibilityToggle();
        }
    }
}