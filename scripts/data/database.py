# -*- coding: utf-8 -*-
'''
    The database.py is responsible for registering data
    from the dataset folder into the MySQL database.
'''

import csv
import datetime
import mysql.connector

__author__ =  'Felipe Fronchetti'
__contact__ = 'fronchetti@usp.br'

class Database():
    def __init__(self, project, inflow):
        openuniversedb = mysql.connector.connect(host="localhost",
                                                 user="root",
                                                 passwd="mechamoluiz",
                                                 db="openuniversedb")

        self.cursor = openuniversedb.cursor()
        self.project = project
        self.inflow = inflow
        self.project_id = self.add_project()
        self.add_statistics()
        self.add_features()
        self.add_newcomers_time_series()
        self.cursor.close()
        openuniversedb.commit()
        openuniversedb.close()

    def add_project(self):
        name = self.project['name']
        owner = self.project['owner']
        owner_type = self.project['owner_type']
        updated_at = datetime.datetime.now()
        created_at = self.project['created_at']
        application_domain = self.project['application_domain']
        software_license = self.project['software_license']
        age = int(self.project['age'])
        main_language = self.project['main_language']
        github_url = self.project['github_url']

        query = ("INSERT INTO website_project "
                 "(name, owner, owner_type, updated_at, created_at, application_domain, software_license, age, main_language, github_url) "
                 "VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s)")

        data = (name, owner, owner_type, updated_at, created_at, application_domain, software_license, age, main_language, github_url)
        self.cursor.execute(query, data)
        project_id = self.cursor.lastrowid

        return project_id

    def add_statistics(self):
        pulls_merged_total = self.project['pulls_merged_total']
        newcomers_total = self.project['newcomers_total']
        open_issues_total = self.project['open_issues_total']
        used_languages_total = self.project['used_languages_total']
        forks_total = self.project['forks_total']
        stars_total =  self.project['stars_total']
        commits_total = self.project['commits_total']
        contributors_total = self.project['contributors_total']
        core_members_total = self.project['core_members_total']

        query = ("INSERT INTO website_projectstatistics "
                 "(project_id, pulls_merged_total, newcomers_total, open_issues_total, used_languages_total, forks_total, stars_total, commits_total, contributors_total, core_members_total) "
                 "VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s)")

        data = (self.project_id, pulls_merged_total, newcomers_total, open_issues_total, used_languages_total, forks_total, stars_total, commits_total, contributors_total, core_members_total)
        self.cursor.execute(query, data)

    def add_features(self):
        has_contributing = self.project['has_contributing']
        has_readme = self.project['has_readme']

        if has_contributing is True:
            has_contributing = 1
        else:
            has_contributing = 0

        if has_contributing is True:
            has_readme = 1
        else:
            has_readme = 0

        query = ("INSERT INTO website_projectfeatures "
                 "(project_id, has_contributing, has_readme) "
                 "VALUES (%s, %s, %s)")

        data = (self.project_id, has_contributing, has_readme)
        self.cursor.execute(query, data)

    def add_newcomers_time_series(self):
        for column in self.inflow:
            if 'project' not in column:
                data_type = 'newcomers'
                date = datetime.datetime.strptime(column + ' 0', "(%W, %Y) %w")
                count = int(self.inflow[column])

                query = ("INSERT INTO website_timeseries "
                         "(project_id, data_type, count, date)"
                         "VALUES (%s, %s, %s, %s)")

                data = (self.project_id, data_type, count, date)
                self.cursor.execute(query, data)


if __name__ == '__main__':
    csv_folder = '../../spreadsheets'

    summary_file = open(csv_folder + '/summary.csv', 'r')
    summary = csv.DictReader(summary_file)

    for project in summary:
        newcomers_inflow_file = open(csv_folder + '/newcomers_inflow.csv', 'r')
        newcomers_inflow = csv.DictReader(newcomers_inflow_file)

        for inflow in newcomers_inflow:
            if inflow['project'] == project['name']:
                database = Database(project, inflow)
