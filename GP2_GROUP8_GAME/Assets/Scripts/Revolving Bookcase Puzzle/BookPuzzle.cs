using System;
using System.Collections;
using System.Collections.Generic;
using TMPro;
using UnityEngine;

public class BookPuzzle : MonoBehaviour
{
     [NonSerialized] public List<char>                   chosenLetters = new List<char>();
     [NonSerialized] public bool                         isResetting;
     public static          Dictionary<GameObject, Book> books = new Dictionary<GameObject, Book>();

     [SerializeField] private string code;
     [SerializeField] private GameObject artifact;
     private                  int      _wrongGuesses;
     private                  Animator _anim;

    //FMOD stuff
    [FMODUnity.EventRef]
    public string BookcaseOpen = "event:/Environmental/bookcaseOpen";
    [FMODUnity.EventRef]
    public string Success = "event:/Music/Success";
    [FMODUnity.EventRef]
    public string Failure = "event:/Music/Failure";

    private void Awake()
     {
          _anim = GetComponent<Animator>();
     }

     public void CompareLetter()
     {
          if (code[chosenLetters.Count - 1] != chosenLetters[chosenLetters.Count - 1] && chosenLetters.Count > 0 && chosenLetters.Count <= code.Length)
          {
               _wrongGuesses++;
          }
          
          if (code.Length == chosenLetters.Count &&
                   _wrongGuesses == 0)
          {
               _anim.SetBool("Open", true);
               artifact.SetActive(true);
               foreach (KeyValuePair<GameObject, Book> entry in books)
               {
                    entry.Value.enabled = false;
                    entry.Key.layer = 0;

                //Play sound
               }
               FMODUnity.RuntimeManager.PlayOneShot(BookcaseOpen, transform.position);
               FMODUnity.RuntimeManager.PlayOneShot(Success);

          }
          else if(code.Length == chosenLetters.Count &&
                  _wrongGuesses > 0)
          {
               chosenLetters = new List<char>();
               _wrongGuesses = 0;
               isResetting = true;
               foreach (KeyValuePair<GameObject, Book> entry in books)
               {
                    if (entry.Value.isChosen)
                    {
                         StartCoroutine(entry.Value.ResetRotation());

                    //Play sound
                    }
               }
               FMODUnity.RuntimeManager.PlayOneShot(Failure);

          }
     } 
}
