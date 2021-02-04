using System;
using System.Collections.Generic;
using Scary_event_System.FMOD_Sound_Event;
using UnityEngine;
using UnityEngine.Events;

namespace Scary_event_System.Custom_Event_System
{
    public class EventManager : MonoBehaviour
    {
        private static EventManager _eventManager;

        private Dictionary<string, UnityEvent> _eventDictionary;

        public static EventManager Instance
        {
            get
            {
                if (!_eventManager)
                {
                    _eventManager = FindObjectOfType(typeof(EventManager)) as EventManager;

                    if (_eventManager)
                    {
                        _eventManager?.Init();
                    }
                }

                return _eventManager;
            }
        }

        void Init()
        {
            if (_eventDictionary == null)
            {
                _eventDictionary = new Dictionary<string, UnityEvent>();
            }
        }

        public static void StartListening(string eventName, UnityAction listener)
        {
            if (Instance._eventDictionary.TryGetValue(eventName, out var thisEvent))
            {
                thisEvent.AddListener(listener);
            }
            else
            {
                thisEvent = new UnityEvent();
                thisEvent.AddListener(listener);
                Instance._eventDictionary.Add(eventName, thisEvent);
            }
        }

        public static void StopListening(string eventName, UnityAction listener)
        {
            if (_eventManager == null) return;
            if (Instance._eventDictionary.TryGetValue(eventName, out var thisEvent))
            {
                thisEvent.RemoveListener(listener);
            }
        }

        public static void TriggerEvent(string eventName)
        {
            if (Instance._eventDictionary.TryGetValue(eventName, out var thisEvent))
            {
                thisEvent.Invoke();
            }
        }
    }

    [DefaultExecutionOrder(-100)]
    public class CustomEventManager
    {
        public delegate void EventListener(EventInfo eventInfo);

        private static CustomEventManager _instace = null;

        private static CustomEventManager CurrentInstance
        {
            get
            {
                if (_instace == null)
                {
                    _instace = new CustomEventManager();
                }

                return _instace;
            }
        }

        private Dictionary<Type, EventListener> EventListeners { get; set; }

        public static void RegisterListener<T>(EventListener eventListener) where T : EventInfo
        {
            if (CurrentInstance.EventListeners == null)
            {
                CurrentInstance.EventListeners = new Dictionary<Type, EventListener>();
            }

            if (!CurrentInstance.EventListeners.ContainsKey(typeof(T)) ||
                CurrentInstance.EventListeners[typeof(T)] == null)
            {
                CurrentInstance.EventListeners[typeof(T)] = eventListener;
                return;
            }

            CurrentInstance.EventListeners[typeof(T)] += eventListener;
        }

        public static void UnregisterListener<T>(EventListener listener) where T : EventInfo
        {
            if (CurrentInstance.EventListeners == null)
            {
                return;
            }

            if (CurrentInstance.EventListeners.ContainsKey(typeof(T)) &&
                CurrentInstance.EventListeners[typeof(T)] != null)
            {
                CurrentInstance.EventListeners[typeof(T)] -= (listener);
            }
        }

        public static void SendNewEvent(EventInfo eventInfo)
        {
            Type eventType = eventInfo.GetType();

            if (CurrentInstance.EventListeners == null || !CurrentInstance.EventListeners.ContainsKey(eventType) ||
                CurrentInstance.EventListeners[eventType] == null)
            {
                return;
            }

            CurrentInstance.EventListeners[eventType](eventInfo);
        }
    }
}