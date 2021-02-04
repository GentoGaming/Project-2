//@Author: Teodor Tysklind / FutureGames / Teodor.Tysklind@FutureGames.nu

using System;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Assertions;

public class BurnerBehaviour : MonoBehaviour, IInteractable
{
    [SerializeField] private GameObject _flame;
    [SerializeField] private GameObject beakerPrefab;
    [SerializeField] private GameObject explosionParticle;
    [SerializeField] private ChemicalCompound[] chemicalCompounds;

    private GameObject _beakerObject;
    private bool _isBurnerActive;
    private float burnerInteractionDistance;
    private ChemicalSpawner spawner;
    private LinkedList<ChemicalType> addedChemicals;

    [FMODUnity.EventRef]
    public string chemistryExplosion = "event:/Environmental/chemistryExplosion";
    [FMODUnity.EventRef]
    public string Failure = "event:/Music/Failure";
    [NonSerialized] public Beaker beaker;

    [Serializable]
    public struct ChemicalCompound
    {
        public ChemicalType[] neededChemicals;
        public Color color;
        public CompoundBehaviour compoundBehaviour;
    }

    public enum CompoundBehaviour
    {
        ACIDIC,
        INGREDIENT_CHEMICAL,
    }

    private ChemicalCompound currentCompound;

    public bool IsBurnerActive
    {
        get => _isBurnerActive;
        private set => _isBurnerActive = value;
    }

    private void Awake()
    {
        CreateEmptyBeaker();
        gameObject.layer = LayerMask.NameToLayer("Interactable");
    }

    private void Update()
    {
        if (_beakerObject == null)
        {
            CreateEmptyBeaker();
        }

        if (Vector3.Distance(_beakerObject.transform.position, transform.position) > 0.5f)
        {
            Debug.Log((Vector3.Distance(_beakerObject.transform.position, transform.position)));
            CreateEmptyBeaker();
        }
    }

    private void Start()
    {
        spawner = FindObjectOfType<ChemicalSpawner>();
    }

    public void Interact()
    {
        ToggleFlame();
    }

    private void ToggleFlame()
    {
        Assert.IsNotNull(_flame);

        if (_isBurnerActive)
        {
            _isBurnerActive = false;
        }
        else
        {
            _isBurnerActive = true;
        }

        _flame.SetActive(_isBurnerActive);
    }

    public void UpdateLiquid(ChemicalType chemical)
    {
        Debug.Log(chemical);
        addedChemicals.AddLast(chemical);

        if (CheckCompound())
        {
            CreateCompound();
        }
        else if (addedChemicals.Count == 3)
        {
            BlowUpCompound();
        }
        else
        {
            beaker.AddChemical();
        }
    }

    private bool CheckCompound()
    {
        foreach (ChemicalCompound chemicalCompound in chemicalCompounds)
        {
            bool rightIngredients = true;

            foreach (ChemicalType chemicalType in chemicalCompound.neededChemicals)
            {
                if (!addedChemicals.Contains(chemicalType))
                {
                    rightIngredients = false;
                }
            }

            if (rightIngredients && chemicalCompound.neededChemicals.Length == addedChemicals.Count)
            {
                currentCompound = chemicalCompound;
                return true;
            }
        }

        return false;
    }

    private void CreateCompound()
    {
        SetCompoundBehaviour(currentCompound.compoundBehaviour);
        _beakerObject.GetComponent<Renderer>().material.color = currentCompound.color;
        spawner.SpawnChemicals();
    }

    private void BlowUpCompound()
    {
        Instantiate(explosionParticle, transform.position, Quaternion.identity);
        FMODUnity.RuntimeManager.PlayOneShot(chemistryExplosion, transform.position);
        FMODUnity.RuntimeManager.PlayOneShot(Failure);
        EmptyBeaker();
        spawner.SpawnChemicals();
    }

    private void EmptyBeaker()
    {
        addedChemicals.Clear();
        beaker.EmptyBeaker();
    }

    private void SetCompoundBehaviour(CompoundBehaviour behaviour)
    {
        if (behaviour == CompoundBehaviour.ACIDIC)
        {
            beaker.AddChemical();
            _beakerObject.AddComponent<AcidBehaviour>();
            beaker.CreateCompound(ChemicalType.ACID);
        }

        if (behaviour == CompoundBehaviour.INGREDIENT_CHEMICAL)
        {
            addedChemicals.Clear();
            addedChemicals.AddFirst(ChemicalType.CL);
            beaker.CreateCompound(ChemicalType.CL);
        }
    }

    private void CreateEmptyBeaker()
    {
        _beakerObject = Instantiate(beakerPrefab, (transform.position + Vector3.up * 0.28f), Quaternion.identity);
        _beakerObject.transform.SetParent(gameObject.transform);
        beaker = _beakerObject.GetComponent<Beaker>();

        addedChemicals = new LinkedList<ChemicalType>();
    }
}