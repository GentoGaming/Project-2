//@Author: Teodor Tysklind / FutureGames / Teodor.Tysklind@FutureGames.nu

using System;
using UnityEngine;

[DefaultExecutionOrder(-100)]

public class GameManager : MonoBehaviour
{
    [NonSerialized] public static GameManager instance;
    [NonSerialized] public GameObject player;
    [NonSerialized] public Inventory inventory;

    [SerializeField] private GameObject _crosshair;
    public static bool LockCursor 
    {
        set { Cursor.lockState = value ? CursorLockMode.Locked : CursorLockMode.None; }
    }

    private void Awake()
    {
        if (instance == null)
        {
            instance = this;
        }
    }
    
    private void Start()
    {
        player = GameObject.FindWithTag("Player");
        inventory = GetComponent<Inventory>();
    }

    private void OnEnable()
    {
        LockCursor = true;
    }

    private void OnDisable()
    {
        LockCursor = false;
    }

    public void ToggleCrosshair(bool value)
    {
        _crosshair.SetActive(value);
    }
}
