using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class LightningTarget : MonoBehaviour
{

    public Transform target;

    public ParticleSystem mySystem;
    public float healthUponArrival = 0.01f;

    public float shortenLifeDistance;
    public float stopDistance;
    public float endSpeed = 10f;


    private ParticleSystem.Particle[] particles;

    private void Update()
    {
        //Makes sure that the particle system is always looking at the target (so that all particles ar initially instantiated in the right direction). All of the code below is for updating the direction of
        //The particles individually.
        transform.LookAt(target.position, transform.up);

        //Creates an array with all the particles in the system. This array is used to alter every single particle inside a for-loop.
        particles = new ParticleSystem.Particle[mySystem.particleCount];

        //gets the amount of living particles this frame, based on the particles in the given particlesystem. This int is used in the for-loop.
        int livingParticles = mySystem.GetParticles(particles);

        for (int i = 0; i < livingParticles; i++)
        {
            //A normalized direction of where I want to send the particle.
            Vector3 direction = (target.position - particles[i].position).normalized;

            //Updates every indivdual particle's position each frame, so that it moves towards the player instead of drifting off randomly, which happens otherwise because of the noise.
            particles[i].position += direction * particles[i].velocity.magnitude * Time.deltaTime;

            //How far a away is the particle from its target?
            float remainingDist = Vector3.Distance(particles[i].position, target.position);

            if (remainingDist <= shortenLifeDistance)
            {
                //Updates the lifetime of the parrticle once it has reached its destination, but only once
                if(particles[i].remainingLifetime > healthUponArrival) 
                {
                    particles[i].remainingLifetime = healthUponArrival;
                }
               

                if (remainingDist <= stopDistance)
                {
                    //Stops the movement of the particle, and adds an artificial movement towards the target (this happens just before the particle dies, to make sure it does not drift away.
                    particles[i].velocity = Vector3.zero;
                    particles[i].position += direction * endSpeed * Time.deltaTime;
                }
            }
        }
        //Sets the particles.
        mySystem.SetParticles(particles, livingParticles);

    }
}
