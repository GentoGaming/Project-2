//@Author: Teodor Tysklind / FutureGames / Teodor.Tysklind@FutureGames.nu

using System;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class Inventory : MonoBehaviour
{
    public List<RawImage> rawEmptyImages;
    [NonSerialized] public int            _count = 0;

    [SerializeField] private List<Texture> icons;
    private                  bool          _chemistryArtefact;

    public bool ChemistryArtefact
    {
        get => _chemistryArtefact;
        set
        {
            _chemistryArtefact = value;
            if (_chemistryArtefact == true)
            {
                AddIcon(1);
            }
        }
    }

    private bool _libraryArtefact;

    
    public bool LibraryArtefact
    {
        get => _libraryArtefact;
        set
        {
            _libraryArtefact = value;
            if (_libraryArtefact == true)
            {
                AddIcon(2 );
            }
        }
    }

    private void AddIcon(int imageIndex)
    {
        rawEmptyImages[_count].texture = icons[imageIndex-1];
        rawEmptyImages[_count].gameObject.SetActive(true);
        _count++;
    }


    private bool _engineeringArtefact;

    public bool EngineeringArtefact
    {
        get => _engineeringArtefact;
        set
        {
            _engineeringArtefact = value;
            if (_engineeringArtefact == true)
            {
                AddIcon(3);
            }
        }
    }
    
    public bool IsInventoryComplete()
    {
        return (_chemistryArtefact && _libraryArtefact && _libraryArtefact) ;
    }
    
    public bool IsNotHoldingAnyArtifact()
    {
        return (!_chemistryArtefact && !_libraryArtefact && !_libraryArtefact) ;
    }


    private void Awake()
    {
        foreach (var image in rawEmptyImages)
        {
            image.gameObject.SetActive(false);
        }
    }
}
