from locust import HttpLocust, TaskSet, task

class WebsiteTasks(TaskSet):
    def on_start(self):
        pass
        # self.client.post("/login", {
        #     "username": "test_user",
        #     "password": ""
        # })

    @task
    def employee_get_list(self):
        res = self.client.get("/api/employee")
        if res.status_code != 200:
            raise Exception("Invalid response")
        print("LUNGHEZZA ARRAY: " + str(len(res.json())))

    @task
    def projects_get_list(self):
        self.client.get("/api/projects")


class WebsiteUser(HttpLocust):
    task_set = WebsiteTasks
    min_wait = 1000
    max_wait = 5000
