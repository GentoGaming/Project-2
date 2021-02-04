using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using UnityEngine;
using Object = System.Object;

namespace Scary_event_System.ScaryEventUI
{
    [Serializable]
    public class CustomSoundClass
    {
        public AudioClip audioClip;
        public GameObject gameObj;
        public float delaySound;
    }

    public class SoundScaryEvent : ScaryEventAbstract
    {
        [HideInInspector] public AudioClip audioClip;
        [HideInInspector] public GameObject gameObj;
        [HideInInspector] public float delaySound;
        [HideInInspector] public List<CustomSoundClass> soundClasses = new List<CustomSoundClass>();

        public List<Object> soundClassses = new List<object>();


        public void AddCustomSound(GameObject go, AudioClip clip, float delay)
        {
            CustomSoundClass soundClass = new CustomSoundClass();
            soundClass.gameObj = go;
            soundClass.audioClip = clip;
            soundClass.delaySound = delay;
            soundClasses.Add(soundClass);
        }


        public override async Task ConditionExecute()
        {
            await Task.Delay((int) (delayEvent * 1000));
            soundClasses.Sort((sound1, sound2) => sound1.delaySound.CompareTo(sound2.delaySound));
            foreach (CustomSoundClass sound in soundClasses)
            {
                await Task.Delay((int) (sound.delaySound * 1000));
                GameObject go = sound.gameObj;
                AudioSource src = go.GetComponent<AudioSource>();
                if (src == null)
                {
                    src = go.AddComponent<AudioSource>();
                    src.Stop();
                    src.playOnAwake = false;
                    src.Stop();
                }

                if (src.clip == null) src.clip = sound.audioClip;
                src.Play();
            }
        }

        public override async Task CustomUpdate()
        {
            // No need for the Custom Update
        }

        public void DeleteSound(CustomSoundClass sound)
        {
            List<CustomSoundClass> temp = soundClasses.ToList();
            temp.Remove(sound);
            soundClasses = temp;
        }
    }
}