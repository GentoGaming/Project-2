//@Author: Teodor Tysklind / FutureGames / Teodor.Tysklind@FutureGames.nu

using System.Collections.Generic;
using UnityEngine;

public class ChemicalSpawner : MonoBehaviour
{
    public Transform spawn1;
    public Transform spawn2;
    public Transform spawn3;
    public Transform spawn4;
    public Transform spawn5;

    public GameObject HG;
    public GameObject B;
    public GameObject N;
    public GameObject C;
    public GameObject NE;

    public LinkedList<Transform> spawns = new LinkedList<Transform>();
    public LinkedList<GameObject> currentChemicals = new LinkedList<GameObject>();
    
    private void Start()
    {
        spawns.AddLast(spawn1);
        spawns.AddLast(spawn2);
        spawns.AddLast(spawn3);
        spawns.AddLast(spawn4);
        spawns.AddLast(spawn5);

        SpawnChemicals();
    }

    public void SpawnChemicals()
    {
        RemoveOldChemicals();

        foreach (Transform spawn in spawns)
        {
            if (spawn == spawn1)
            {
                GameObject go = Instantiate(NE, spawn.position, spawn.localRotation);
                go.transform.SetParent(transform);
                currentChemicals.AddLast(go);
            }
            else if (spawn == spawn2)
            {
                GameObject go = Instantiate(HG, spawn.position, spawn.localRotation);
                go.transform.SetParent(transform);
                currentChemicals.AddLast(go);
            }
            else if (spawn == spawn3)
            {
                GameObject go = Instantiate(B, spawn.position, spawn.localRotation);
                go.transform.SetParent(transform);
                currentChemicals.AddLast(go);
            }
            else if (spawn == spawn4)
            {
                GameObject go = Instantiate(N, spawn.position, spawn.localRotation);
                go.transform.SetParent(transform);
                currentChemicals.AddLast(go);
            }
            else if (spawn == spawn5)
            {
                GameObject go = Instantiate(C, spawn.position, spawn.localRotation);
                go.transform.SetParent(transform);
                currentChemicals.AddLast(go);
            }
        }
    }
    
    private void RemoveOldChemicals()
    {
        foreach (GameObject go in currentChemicals)
        {
            Destroy(go);
        }

        currentChemicals.Clear();
    }
}
