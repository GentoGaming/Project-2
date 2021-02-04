using UnityEngine;

namespace Scary_event_System.FMOD_Sound_Event
{
    public class EventInfo
    {
        private EventInfo(GameObject senderObject = null, string description = "")
        {
            GO = senderObject;
            Description = description;
        }

        private GameObject GO { get; set; }
        private string Description { get; set; }

        public class FmodEventInfo : EventInfo
        {
            public FmodEventInfo(float targetPresenceValue, float targetAmbienceValue, GameObject senderObject = null,
                string description = "Calling FMOD Library to change Values of the music and the ambience") : base(
                senderObject,
                description)
            {
                TargetPresenceValue = targetPresenceValue;
                TargetAmbienceValue = targetAmbienceValue;
            }

            public float TargetPresenceValue { get; private set; }
            public float TargetAmbienceValue { get; private set; }

            public string Name { get; private set; }
        }
    }
}