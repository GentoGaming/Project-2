using System.Collections;
using UnityEngine;
using UnityEngine.UI;

public class ArtifactPedestal : MonoBehaviour, IInteractable
{
    [SerializeField] private GameObject[] artifactsInBowl;
    [SerializeField] private ParticleSystem weakSmoke;
    [SerializeField] private ParticleSystem mysticSmoke;
    private                  int          _artefacts;
    [SerializeField] private Animator _outro;
    [SerializeField] private float _delay = 4f;

    public void Interact()
    {
        if (GameManager.instance.inventory.IsNotHoldingAnyArtifact())
        {
            if (!weakSmoke.isPlaying)
            {
                weakSmoke.Play();
            }
        }

        if (GameManager.instance.inventory.EngineeringArtefact == true)
        {
            GameManager.instance.inventory.EngineeringArtefact = false;
            artifactsInBowl[0].SetActive(true);
        }
        if (GameManager.instance.inventory.LibraryArtefact == true)
        {
            GameManager.instance.inventory.LibraryArtefact = false;
            artifactsInBowl[1].SetActive(true);
        }
        if (GameManager.instance.inventory.ChemistryArtefact == true)
        {
            GameManager.instance.inventory.ChemistryArtefact = false;
            artifactsInBowl[2].SetActive(true);
        }
        
        foreach (RawImage r in GameManager.instance.inventory.rawEmptyImages)
        {
            if (r.gameObject.activeSelf)
            {
                r.gameObject.SetActive(false);
                GameManager.instance.inventory._count = 0;
                _artefacts++;
            }
        }

        if (_artefacts > 2)
        {
            if (!mysticSmoke.isPlaying)
            {
                mysticSmoke.Play();
                StartCoroutine(OutroDelay());
            }
        }
    }

    IEnumerator OutroDelay() 
    {
        yield return new WaitForSeconds(_delay);

        _outro.SetTrigger("End");
    }

}
