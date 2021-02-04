//@Author: Teodor Tysklind / FutureGames / Teodor.Tysklind@FutureGames.nu

using System.Collections;
using UnityEngine;

public class Beaker : MonoBehaviour
{
    [FMODUnity.EventRef]
    public string Success = "event:/Music/Success";

    [SerializeField] private Transform _liquid;
    [SerializeField] private float fillRate = 0.2f;
    private Vector3 _defaultScale;
    private int _noOfChemicals = default;
    private const int MAX_CHEMICALS = 3;

    private void Start()
    {
        _defaultScale = _liquid.localScale;
        EmptyBeaker();
    }

    public void EmptyBeaker()
    {
        StopCoroutine(AnimateFill());
        _liquid.localScale = new Vector3(_defaultScale.x, 0, _defaultScale.z);
        _noOfChemicals = 0;
    }

    public void AddChemical()
    {
        _noOfChemicals++;
        StartCoroutine(AnimateFill());
    }

    private IEnumerator AnimateFill()
    {
        float currentY = _liquid.localScale.y;
        float targetY = _defaultScale.y / MAX_CHEMICALS * _noOfChemicals;

        while (currentY < targetY)
        {
            currentY += fillRate * Time.deltaTime;
            _liquid.localScale = new Vector3(_defaultScale.x, currentY, _defaultScale.z);

            yield return null;
        }
    }

    public void CreateCompound(ChemicalType type)
    {
        if (type == ChemicalType.CL)
        {
            _liquid.GetComponentInChildren<Renderer>().material.color = Color.cyan;
        }
        else
        {
            _liquid.GetComponentInChildren<Renderer>().material.color = Color.green;
        }
        
        FMODUnity.RuntimeManager.PlayOneShot(Success);
    }
}
