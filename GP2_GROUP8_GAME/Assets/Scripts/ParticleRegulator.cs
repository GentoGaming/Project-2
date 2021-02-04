using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ParticleRegulator : MonoBehaviour
{
    [SerializeField] private float                      strengthMin;
    [SerializeField] private float                      strengthMax;
    [SerializeField] private float                      frequency;
    private                  ParticleSystem             ps;
    private                  ParticleSystem.NoiseModule noise;
    // Start is called before the first frame updat
    #if UNITY_EDITOR
    void Start()
    {
        ParticleSystem ps = GetComponent<ParticleSystem>();
        noise = ps.noise;
        noise.frequency = frequency;
    }

    // Update is called once per frame
    void Update()
    {
        noise.strength = Random.Range(strengthMin, strengthMax);
    }
    #endif
}
