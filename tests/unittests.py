import unittest
import requests
import random
import shutil

class MockServerTests(unittest.TestCase):
    URL = "http://localhost:8080"
    def setUp(self):
        shutil.copy2('test.data.json', '../bin/data.json')
        self.resname = self.URL + "/api/resource{:04}".format(random.randrange(1000))

    def __deleteres(self):
        self.assertEqual(200, requests.delete(self.resname).status_code)

    def test_projects_get(self):
        resp = requests.get(self.URL + '/api/projects')
        self.assertEqual(200, resp.status_code)
        self.assertEqual('application/json; charset=utf-8', resp.headers['Content-Type'])
        self.assertEqual(2, len(resp.json()['data']))

    def test_check_empty_collection(self):
        self.__deleteres()
        resp = requests.get(self.resname)
        self.assertEqual(0, len(resp.json()["data"]))

    def test_create_entity(self):
        testdict = {"key1":"value2", "key2": "value2"}
        self.__deleteres()
        resp = requests.post(self.resname, json = testdict);
        self.assertEqual(201, resp.status_code)
        xref = resp.headers['X-REF']
        self.assertIsNotNone(xref)
        resp = requests.get(self.URL + xref)
        self.assertEqual(200, resp.status_code)
        json = resp.json()['data']
        self.assertTrue("_oid" in json, "Object doesn't contain '_oid'")
        for k in testdict:
            self.assertTrue(k in json)
            self.assertEqual(testdict[k], json[k])

    def test_create_multiple_entity(self):
        self.__deleteres()
        for i in range(10):
            d = dict()
            d['key'] = "value{:02}".format(i)
            resp = requests.post(self.resname, json = d)
            self.assertEqual(201, resp.status_code)
            xref = resp.headers['X-REF']
            self.assertIsNotNone(xref)

        resp = requests.get(self.resname)
        self.assertEqual(200, resp.status_code)
        self.assertEqual(10, len(resp.json()['data']))

    def test_update_entity(self):
        self.test_create_multiple_entity()
        resp = requests.get(self.resname)
        jobj = resp.json()['data'][0]
        oid = jobj['_oid']
        k = jobj['_oid']
        jobj['key'] = "changed"
        resp = requests.put(self.resname + '/' + oid, json=jobj)
        self.assertEqual(200, resp.status_code)
        resp = requests.get(self.resname + '/' + oid)
        self.assertEqual("changed", resp.json()['data']['key'])

if __name__ == '__main__':
    unittest.main()