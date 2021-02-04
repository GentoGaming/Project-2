using UnityEngine;

namespace Electric_Puzzle
{
    public class ElectricNode
    {
        public ParticleSystem electricPS;
        public bool isConnected;
        public LightningTarget lightningTargetScript;
        public ElectricNode next = null;

        public GameObject thisObj;
    }
}