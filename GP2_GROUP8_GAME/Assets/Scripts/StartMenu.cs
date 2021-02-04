//@Author: Teodor Tysklind / FutureGames / Teodor.Tysklind@FutureGames.nu

using System.Collections;
using UnityEngine;
using UnityEngine.SceneManagement;
using UnityEngine.UI;
using UnityEngine.Video;

public class StartMenu : MonoBehaviour
{
    [SerializeField] private GameObject AudioObject;
    
    [SerializeField] private GameObject _rainPlayer;
    [SerializeField] private GameObject _bgPlayer;

    [SerializeField] private RawImage _rainImage;
    [SerializeField] private RawImage _bgImage;
    [SerializeField] private RawImage _blackBGImage;
    [SerializeField] private RawImage _blackOverlay;
    [SerializeField] private RawImage _cathedral;
    [SerializeField] private GameObject Cathedral;

    private VideoPlayer _rain;
    private VideoPlayer _bg;
    private AudioSource _audio;

    private Animator transitionAnimator;
    
    private bool isPlaying = false;

    private float blackOverlayOpacity;
    
    private void Awake()
    {
        _audio = AudioObject.GetComponent<AudioSource>();
        _rain = _rainPlayer.GetComponent<VideoPlayer>();
        _bg = _bgPlayer.GetComponent<VideoPlayer>();
        transitionAnimator = Cathedral.GetComponent<Animator>();

        _blackOverlay.gameObject.SetActive(false);

        _rainImage.gameObject.SetActive(false);
        _bgImage.gameObject.SetActive(false);
        _blackBGImage.gameObject.SetActive(true);
        _cathedral.gameObject.SetActive(true);

        _rain.Prepare();
        _bg.Prepare();
    }

    public void StartAnimation()
    {

        if (_rain.isPrepared && _bg.isPrepared)
        {
            _rainImage.gameObject.SetActive(true);
            _bgImage.gameObject.SetActive(true);
            _blackBGImage.gameObject.SetActive(false);
            
            _blackOverlay.gameObject.SetActive(true);


            _bg.Play();
            _rain.Play();
            _audio.Play();

            StartCoroutine(QueueAnimationEvents());
        }
    }

    private IEnumerator QueueAnimationEvents()
    {
        while (_audio.isPlaying)
        {
            yield return null;
        }
        
        transitionAnimator.SetTrigger("Zoom");


        while (transitionAnimator.GetCurrentAnimatorStateInfo(0).IsName("Idle"))
        {
            yield return null;
        }
        
        while (transitionAnimator.GetCurrentAnimatorStateInfo(0).IsName("Zooming"))
        {
            yield return null;
        }
        
        SceneManager.LoadScene("AlchemyScene");
    }
}
