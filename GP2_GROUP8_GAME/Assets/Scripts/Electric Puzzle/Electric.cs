using System.Collections;
using System.Collections.Generic;
using Scary_event_System.Custom_Event_System;
using UnityEngine;

namespace Electric_Puzzle
{
    public class Electric : MonoBehaviour
    {
        public float powerRange = 3f;
        public string firstNodeName = "RootNode";
        public GameObject prefabFinish;
        public float delayOn = 0.2f;

        private List<GameObject> _allGameObjects = new List<GameObject>();
        private PressingButtonInteaction _button;
        private ElectricNode _currentNode;
        private GameObject _dropLocation;
        private GameObject _finalDestination;
        private ElectricNode _finalNode;
        private bool _isPuzzleEnded ;
        private bool _isPuzzleSolved;
        private ElectricNode _nextNode;
        private ElectricNode _previousNode;
        private ElectricNode _rootNode;


        private void Awake()
        {
            _button = GameObject.Find("E_Button").GetComponent<PressingButtonInteaction>();
            _dropLocation = GameObject.Find("Drop Location");
            GameObject root = transform.Find(firstNodeName).gameObject;
            _finalDestination = GameObject.Find("Final Destination");
            if (root == null)
            {
                Debug.LogWarning("No Child in puzzle game is set to Root node !" +
                                 " Please choose 1 of the nodes to be a root . @dev Maroun");
            }


            _rootNode = new ElectricNode();
            FillUpDataAtStart(_rootNode, root);
            _finalNode = new ElectricNode();
            FillUpDataAtStart(_finalNode, _finalDestination);


            _currentNode = _rootNode;
            foreach (Transform child in transform)
            {
                if (child.gameObject == _rootNode.thisObj) continue;
                GameObject childGameObject = child.gameObject;
                _allGameObjects.Add(childGameObject);
                _currentNode.next = new ElectricNode();
                FillUpDataAtStart(_currentNode.next, childGameObject);
                _currentNode = _currentNode.next;
            }

            _currentNode.next = _finalNode;
            _allGameObjects.Add(_finalDestination);
        }

        private void Update()
        {
            if (_button.buttonState == false)
            {
                EnsureAllDevicesAreOff();
                return;
            }

            StartCoroutine(TurnOnOff());
        }
        

        private void FillUpDataAtStart(ElectricNode node, GameObject go)
        {
            node.thisObj = go;
            node.lightningTargetScript = go.GetComponentInChildren<LightningTarget>();
            node.electricPS = go.GetComponentInChildren<ParticleSystem>();
        }


        int Count(ElectricNode list)
        {
            ElectricNode tempNode = list;
            int nodes = 0;

            while (tempNode != null)
            {
                nodes++;
                tempNode = tempNode.next;
            }

            return nodes;
        }

        GameObject GetClosestGameObject(List<GameObject> newList, GameObject go)
        {
            float maxDistance = Mathf.Infinity;
            GameObject potentialGameObject = null;


            foreach (var gObject in _allGameObjects)
            {
                if (go == gObject || (newList != null && newList.Contains(gObject))) continue;

                float calcDist = Vector3.Distance(go.transform.position, gObject.transform.position);
                if (calcDist < maxDistance)
                {
                    maxDistance = calcDist;
                    potentialGameObject = gObject;
                }
            }

            return potentialGameObject;
        }

        List<GameObject> SortGameObjects()
        {
            List<GameObject> newList = new List<GameObject>();
            newList.Add(_rootNode.thisObj);
            int counter = 0;
            foreach (var go in _allGameObjects)
            {
                GameObject getObj = GetClosestGameObject(newList, newList[counter]);
                if (getObj != null)
                {
                    newList.Add(getObj);
                    counter++;
                }
            }

            newList.Add(_finalDestination);
            return newList;
        }


        private int FindPosition(GameObject obj)
        {
            ElectricNode tempNode = _rootNode;
            int countingPos = 0;
            while (tempNode != null)
            {
                if (tempNode.thisObj == obj) return countingPos;
                countingPos++;
                tempNode = tempNode.next;
            }

            return 0;
        }


        private void UpdateLinkedList()
        {
            List<GameObject> newObjList = SortGameObjects();


            _currentNode = _rootNode;
            int counter = 0;
            int pos1 = 0;
            int pos2 = 0;


            while (_currentNode != null)
            {
                if (_currentNode.thisObj != newObjList[counter])
                {
                    pos1 = counter;
                    pos2 = FindPosition(newObjList[counter]);
                    Swap(pos1 + 1, pos2 + 1);
                    break;
                }
                else
                {
                    _currentNode = _currentNode.next;
                    counter++;
                }
            }
        }

        private void EnsureAllDevicesAreOff()
        {
            ElectricNode tempNode = _rootNode;
            while (tempNode != null)
            {
                tempNode.lightningTargetScript.enabled = false;
                tempNode.lightningTargetScript.target = null;
                tempNode.electricPS.Stop();
                tempNode = tempNode.next;
            }
        }


        IEnumerator DelayPlay(float delayTime, ParticleSystem ps)
        {
            yield return new WaitForSeconds(delayTime);
            ps.Play();
        }


        IEnumerator TurnOnOff()
        {
            UpdateLinkedList();

            ElectricNode tempNode = _rootNode;
            while (tempNode != null)
            {
                float delayTime = delayOn;
                if (tempNode == _rootNode) delayTime = 0;
                yield return new WaitForSeconds(delayTime);

                if (tempNode.next != null)
                {
                    float distance = Vector3.Distance(tempNode.thisObj.transform.position,
                        tempNode.next.thisObj.transform.position);
                    if (distance < powerRange)
                    {
                        if (!tempNode.next.isConnected) tempNode.next.isConnected = true;
                        if (!Equals(tempNode.lightningTargetScript.target,
                            tempNode.next.thisObj.transform.GetChild(0).transform))
                            tempNode.lightningTargetScript.target =
                                tempNode.next.thisObj.transform.GetChild(0).transform;
                        if (!tempNode.lightningTargetScript.enabled) tempNode.lightningTargetScript.enabled = true;
                        if (!tempNode.electricPS.isPlaying)
                        {
                            tempNode.electricPS.Play();
                        }
                    }
                    else
                    {
                        break;
                    }
                }

                tempNode = tempNode.next;
            }

            // All the rest are not linked , if there is left iteration we will shut them down
            while (tempNode != null)
            {
                if (tempNode.next != null) tempNode.next.isConnected = false;
                tempNode.lightningTargetScript.enabled = false;
                tempNode.lightningTargetScript.target = null;
                tempNode.electricPS.Stop();
                tempNode = tempNode.next;
            }


            if (_finalNode.isConnected)
            {
                _isPuzzleSolved = true;
            }

            if (_isPuzzleSolved == true && _isPuzzleEnded == false)
            {
                _isPuzzleEnded = true;
                GameObject obj = Instantiate(prefabFinish);
                obj.transform.position = _dropLocation.transform.position;
                EventManager.TriggerEvent("DisableEleButton");
            }
        }


        public void IterateList()
        {
            ElectricNode tempNode = _rootNode;
            while (tempNode != null)
            {
                Debug.Log("Nodes : " + tempNode.thisObj.name);
                tempNode = tempNode.next;
            }
        }


        int Swap(int pos1, int pos2)
        {
            ElectricNode node1 = null, node2 = null, prev1 = null, prev2, temp = null;
            int i;
            int maxPos = pos1 > pos2 ? pos1 : pos2;

            // Get total nodes in the list

            int totalNodes = Count(_rootNode);

            // Validate swap positions
            if ((pos1 <= 0 || pos1 > totalNodes) || (pos2 <= 0 || pos2 > totalNodes))
            {
                return -1;
            }

            // If both positions are same then no swapping required
            if (pos1 == pos2)
            {
                return 1;
            }


            // Identify both nodes to swap
            i = 1;
            temp = _rootNode;
            prev1 = null;
            prev2 = null;

            // Find nodes to swap
            while (temp != null && (i <= maxPos))
            {
                if (i == pos1 - 1)
                    prev1 = temp;
                if (i == pos1)
                    node1 = temp;

                if (i == pos2 - 1)
                    prev2 = temp;
                if (i == pos2)
                    node2 = temp;

                temp = temp.next;
                i++;
            }

            // If both nodes to swap are found.
            if (node1 != null && node2 != null)
            {
                // Link previous of node1 with node2
                if (prev1 != null)
                    prev1.next = node2;

                // Link previous of node2 with node1
                if (prev2 != null)
                    prev2.next = node1;

                // Swap node1 and node2 by swapping their 
                // next node links
                temp = node1.next;
                node1.next = node2.next;
                node2.next = temp;
            }

            return 1;
        }
    }
}