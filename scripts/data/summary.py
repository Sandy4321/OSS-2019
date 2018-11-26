#!/usr/bin/env python
# -*- coding: utf-8 -*-

__author__ =  'Felipe Fronchetti'
__contact__ = 'fronchetti@usp.br'

# TO-DO: Core contributors are found using merged pull-requests. The problem is, not all the projects are using the pull-request system.

import os
import csv
import json
import numpy
from datetime import timedelta
from datetime import datetime
from dateutil.relativedelta import relativedelta

class Summary():
    def __init__(self, repository, folder):
        self.folder = folder
        self.domains = {}

        with open('../../spreadsheets/domains.csv', 'r') as domains_file:
            domains = csv.DictReader(domains_file)

            for domain in domains:
                self.domains[domain['name']] = domain['domain']

    def get_pull_requests_merged(self):
        pull_requests_file = json.load(open(self.folder + '/pull_requests.json', 'r'))
        merged_list = []

        for line in pull_requests_file:
            merged_date =  line['merged_at']

            if merged_date is not None:
                merged_date = datetime.strptime(merged_date, '%Y-%m-%dT%H:%M:%SZ').date()
                merged_list.append(merged_date)

        return merged_list

    def get_merged_code_churn(self):
        pull_requests_file = json.load(open(self.folder + '/pull_requests.json', 'r'))
        additions_minus_deletions = []

        for line in pull_requests_file:
            merged_date =  line['merged_at']

            if merged_date is not None:
                if 'additions' in line and 'deletions' in line:
                    lines_added = int(line['additions'])
                    lines_deleted = int(line['deletions'])

                    additions_minus_deletions.append(lines_added - lines_deleted)

        code_churn = sum(additions_minus_deletions)
        return code_churn

    def get_time_for_merge(self):
        pull_requests_file = json.load(open(self.folder + '/pull_requests.json', 'r'))
        time_for_merge = []

        for line in pull_requests_file:
            merged_date =  line['merged_at']

            if merged_date is not None:
                merged_date = datetime.strptime(merged_date, '%Y-%m-%dT%H:%M:%SZ').date()
                created_at = datetime.strptime(line['created_at'], '%Y-%m-%dT%H:%M:%SZ').date()
                interval = abs(merged_date - created_at).days
                time_for_merge.append(interval)

        return time_for_merge

    def get_time_for_first_review(self):
        # TO-DO
        return True

    def get_reviews(self):
        # TO-DO
        return True

    def get_reviews(self):
        # TO-DO
        return True

    def get_core_contributors(self):
        pull_requests_file = json.load(open(self.folder + '/pull_requests.json', 'r'))
        core_contributors = []

        for line in pull_requests_file:
            if 'merged_by' in line:
                merged_by = line['merged_by']

                if merged_by is not None:
                    if 'login' in line['merged_by']:
                        core_member = line['merged_by']['login']
                    else:
                        core_member = 'Anonymous'
                    
                    if core_member not in core_contributors:
                        core_contributors.append(core_member)

        return core_contributors

    def get_commits(self):
        commits_file = json.load(open(self.folder + '/commits.json', 'r'))
        commits_list = []

        about_file = json.load(open(self.folder + '/about.json', 'r'))
        created_at = datetime.strptime(about_file['created_at'], '%Y-%m-%dT%H:%M:%SZ') + relativedelta(months=6)

        for line in commits_file:
            commit_date = line['commit']['author']['date']

            if commit_date is not None:
                commit_date = datetime.strptime(commit_date, '%Y-%m-%dT%H:%M:%SZ').date()

                if commit_date >= created_at.date():
                    commits_list.append(commit_date)

        return commits_list

    def get_newcomers(self):
        commits_file = json.load(open(self.folder + '/commits.json', 'r'))
        newcomers_list = []

        about_file = json.load(open(self.folder + '/about.json', 'r'))
        created_at = datetime.strptime(about_file['created_at'], '%Y-%m-%dT%H:%M:%SZ') + relativedelta(months=6)

        for line in commits_file:
            newcomer = line['commit']['author']['name']
            commit_date = line['commit']['author']['date']

            if commit_date is not None:
                if newcomer is not None:
                    commit_date = datetime.strptime(commit_date, '%Y-%m-%dT%H:%M:%SZ').date()

                    if commit_date >= created_at.date():
                        if newcomer not in newcomers_list:
                            newcomers_list.append(newcomer)

        return newcomers_list

    def get_contributors(self):
        commits_file = json.load(open(self.folder + '/commits.json', 'r'))
        contributors_list = []

        for line in commits_file:
            contributor = line['commit']['author']['name']

            if contributor is not None:
                if contributor not in contributors_list:
                    contributors_list.append(contributor)

        return contributors_list        

    def get_stars(self):
        stars_file = json.load(open(self.folder + '/stars.json', 'r'))
        stars_list = []

        for line in stars_file:
            star_date = line['starred_at']

            if star_date is not None:
                star_date = datetime.strptime(star_date, '%Y-%m-%dT%H:%M:%SZ').date()
                stars_list.append(star_date)

        return stars_list

    def get_forks(self):
        forks_file = json.load(open(self.folder + '/forks.json', 'r'))
        forks_list = []

        for line in forks_file:
            fork_date = line['created_at']

            if fork_date is not None:
                fork_date = datetime.strptime(fork_date, '%Y-%m-%dT%H:%M:%SZ').date()
                forks_list.append(fork_date)

        return forks_list

    def get_domain(self):
        if repository['full_name'] in self.domains.keys():
            domain = self.domains[repository['full_name']]
        else:
            domain = 'Other'
        return domain

    def get_license(self):
        about_file = json.load(open(self.folder + '/about.json', 'r'))

        if 'license' in about_file:
            if about_file['license'] is not None:
                return about_file['license']['name']
        return 'Other'

    def get_used_languages(self):
        used_languages = json.load(open(self.folder + '/languages.json', 'r'))
        return used_languages

    def has_readme(self):
        readme = json.load(open(self.folder + '/readme.json', 'r'))

        if readme:
            return True
        else:
            return False

    def has_contributing(self):
        contributing = json.load(open(self.folder + '/contributing.json', 'r'))

        if contributing:
            return True
        else:
            return False

if __name__ == '__main__':
    dataset_folder = '../../dataset'
    csv_folder = '../../spreadsheets'

    if os.path.isfile(dataset_folder + '/projects.json'):
        with open(dataset_folder + '/projects.json', 'r') as projects_file:
            projects = json.load(projects_file)

    fieldnames = ['name',
                  'owner',
                  'created_at',
                  'github_url',
                  'pulls_merged_total',
                  'commits_total',
                  'stars_total',
                  'forks_total',
                  'has_contributing',
                  'has_readme',
                  'used_languages_total',
                  'open_issues_total',
                  'age',
                  'application_domain',
                  'main_language',
                  'owner_type',
                  'software_license',
                  'newcomers_total',
                  'contributors_total',
                  'core_contributors_total',
                  'pull_merged_code_churn',
                  'time_for_first_review_median',
                  'time_for_merge_median']

    with open(csv_folder + '/summary.csv', 'w') as summary_file:
        writer = csv.DictWriter(summary_file, fieldnames=fieldnames)
        writer.writeheader()

    for language in projects.keys():
        repositories = projects[language]['items']

        for repository in repositories:
            project_folder = dataset_folder + '/' + language + '/' + repository['name']
            project = Summary(repository, project_folder)

            created_at = datetime.strptime(repository['created_at'], '%Y-%m-%dT%H:%M:%SZ').date()
            pull_merged_total = project.get_pull_requests_merged()
            commit_total = project.get_commits()
            star_total = project.get_stars()
            fork_total = project.get_forks()
            open_issues = repository['open_issues_count']
            used_languages = project.get_used_languages()
            has_contributing = project.has_contributing()
            has_readme = project.has_readme()
            owner_type = repository['owner']['type']
            main_language = repository['language']
            age = 2018 - int(created_at.year)
            application_domain = project.get_domain()
            software_license = project.get_license()
            newcomers = project.get_newcomers()
            contributors = project.get_contributors()
            core_contributors = project.get_core_contributors()
            pull_merged_code_churn = project.get_merged_code_churn()
            time_for_first_review = project.get_time_for_first_review()
            time_for_merge = project.get_time_for_merge()

            with open(csv_folder + '/summary.csv', 'a') as summary_file:
                writer = csv.DictWriter(summary_file, fieldnames=fieldnames)
                data = {'name': repository['name'],
                        'owner': repository['owner']['login'],
                        'created_at': created_at,
                        'github_url': repository['html_url'],
                        'pulls_merged_total': len(numpy.nan_to_num(pull_merged_total)),
                        'commits_total': len(numpy.nan_to_num(commit_total)),
                        'stars_total': len(numpy.nan_to_num(star_total)),
                        'forks_total': len(numpy.nan_to_num(fork_total)),
                        'has_contributing': has_contributing,
                        'has_readme': has_readme,
                        'used_languages_total': len(used_languages),
                        'open_issues_total': open_issues,
                        'age': age,
                        'application_domain': application_domain,
                        'main_language': main_language,
                        'owner_type': owner_type,
                        'software_license': software_license,
                        'newcomers_total': len(numpy.nan_to_num(newcomers)),
                        'contributors_total': len(numpy.nan_to_num(contributors)),
                        'core_contributors_total': len(numpy.nan_to_num(core_contributors)),
                        'pull_merged_code_churn': pull_merged_code_churn,
                        'time_for_first_review_median': numpy.median(numpy.nan_to_num(time_for_first_review)),
                        'time_for_merge_median': numpy.median(numpy.nan_to_num(time_for_merge))}

                writer.writerow(data)
