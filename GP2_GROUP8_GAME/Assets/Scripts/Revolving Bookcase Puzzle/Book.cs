using System;
using System.Collections;
using System.Collections.Generic;
using TMPro;
using UnityEngine;

public class Book : MonoBehaviour, IInteractable
{
    [NonSerialized] public bool isChosen;

    [SerializeField] private char        letter;
    [SerializeField] private float       rotationMagnitude;
    private                  BookPuzzle  _bookDoor;
    private                  Transform   _parentTransform;
    private                  TextMeshPro _textMesh;

    //FMOD stuff
    [FMODUnity.EventRef]
    public string BookPull = "event:/Environmental/bookcasePull";

    private void Awake()
    {
        _bookDoor = transform.parent.parent.gameObject.GetComponent<BookPuzzle>();
        _parentTransform = transform.parent;
        _textMesh = _parentTransform.GetChild(1).GetComponent<TextMeshPro>();
        BookPuzzle.books.Add(this.gameObject, this.GetComponent<Book>());
    }

    public void Interact()
    {
        if (!_bookDoor.isResetting && !isChosen)
        {
            _bookDoor.chosenLetters.Add(letter);
            StartCoroutine(RotateObject());
            _bookDoor.CompareLetter();

            //Play sound
            FMODUnity.RuntimeManager.PlayOneShot(BookPull, transform.position);
        }
    }

    public IEnumerator ResetRotation()
    {
        yield return new WaitForSeconds(1);
        float startTime = 0.1f;
        while(startTime < 3)
        {
            _parentTransform.localRotation = Quaternion.Lerp(_parentTransform.localRotation, Quaternion.Euler(0, 0, 0), (startTime += Time.deltaTime)/85f);
            yield return null;
        }

        _bookDoor.isResetting = false;
        isChosen = false;
    }
    
    //TODO adapt this method
    private IEnumerator RotateObject()
    {
        isChosen = true;
        float startTime = Time.time;
        while(Time.time < startTime + 2f)
        {
            _parentTransform.localRotation = Quaternion.Lerp(_parentTransform.localRotation, Quaternion.Euler(rotationMagnitude, 0, 0), (Time.time - startTime)/5f);
            yield return null;
        }
    }
}
