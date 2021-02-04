using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;

public class IntroSwitch : MonoBehaviour
{
    private AudioSource audioS;

    public AudioClip firstHalf, secondHalf, thunder;
   
    void Awake()
    {
        audioS = GetComponent<AudioSource>();


    }

    public void SwitchSound(int sound)
    {
        switch (sound) 
        {
            case 1:
                audioS.PlayOneShot(firstHalf);
                break;
            case 2:
                audioS.PlayOneShot(secondHalf);
                break;
            case 3:
                audioS.PlayOneShot(thunder);
                break;
        }

    }

    public void LoadNextScene()
    {
        SceneManager.LoadScene("Main_Scene");
    }
}
