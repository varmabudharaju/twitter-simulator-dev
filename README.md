# DOSP Project 4 Part 1: Twitter Simulator
****
## Group Details
<p><strong>Name: [ Pradyumna_Pasumarty : Sai Ram Varma_Budharaju ]</strong></p>
<strong>Members</strong>
<ol>
    <li>Sai Ram Varma Budharaju (UFID: 3337-4276 | <a>sbudharaju@ufl.edu</a>)</li>
    <li>Pradyumna Pasumarty (UFID: 6907-9707 | <a>ppasumarty@ufl.edu</a>)</li>
</ol>

****

## Application Description

The main purpose of this application is to achieve the core functionalities of Twitter. 
Additionally, the application also has functionalities for simulation and testing the features and their relative performances.
Lastly, a Client Server & Middleware architecture was employed to simulate the realistic usage of this application.

****

## Design Details

<ul>
    <li>
        Middleware Design
        <ul>
            <li>The middleware component mainly provides the capability to initialize the application either as a Server, Client or a Simulator.</li>
            <li>Based on the functionality requested, the middleware component then sets up the appropriate Connections & the respective Sockets</li>
            <li>Post successful connection, the Sockets act as utilities to communicate requests and responses among clients and the server</li>
        </ul>
    </li>
    <br/>
    <li>
        Client Design
        <ul>
            <li>The client module has functionalities modelling the Twitter User-Application</li>
            <li>These capabilities include letting a User choose options like</li>
                <ul>
                    <li>UserName Registration</li>
                    <li>Subscribe</li>
                    <li>Unsubscribe</li>
                    <li>Retweet</li>
                    <li>Tweet queries based on hashtags and mentions</li>
                </ul>
            <li>Lastly, the client provides a live display of tweets from users that the client is subscribed to.</li>
        </ul>
    </li>
    <br/>
    <li>
        Server Design
        <ul>
            <li>The Server implements all the functionalities requested by the clients and relays data using Socket communications</li>
            <li>It also stores ETS Counters to keep track of the activity statistics such as number of tweets, online/offline users and total users simulated.</li>
            <li>It stores data related to each client, their socket, their subscriptions and tweets using ETS Tables</li>
            <li>On receiving tweets, the Server first parses them to extract hashtag and mentions information and updates these relationships in ETS Tables for future queries</li>
            <li>Then, the Server retrieves the subscribers to be notified, and communicates the tweets to all of these subscribers.</li>
        </ul>
    </li>
    <br/>
    <li>
        Simulator Design
        <ul>
            <li>The Simulator is provided as a submodule of the Client Module as it has the same functionalities.</li>
            <li>The Simulator has the additional capability of generating multiple instances of a client based on console input.</li>
            <li>The key feature of this simulator is to use ZipF Distribution to create users in a realistic social media model.</li>
            <li>Specifically, a small percentile of users has a large subscriber base and thus have high tweet frequencies. Furthermore, their offline periods are limited</li>
            <li>A larger percentile of users has minimal to no subscriber bases and don't tweet or login frequently enough</li>
            <li>Lastly, the simulator can randomly generate strings on behalf of the users and tweet them to their subscribers</li>
        </ul>
    </li>
    <br/>
    <li>
        Activity Monitoring
        <ul>
            <li>The Activity Monitor is a submodule of the Server</li>
            <li>This feature continuously monitors if the users are online or offline and if they have tweeted recently</li>
            <li>It periodically displays these stats in the Server console for a quick lookup</li>
            <li>Furthermore, these stats are written to a text file for plotting</li>
        </ul>
    </li>
</ul>

****

## Setup and Execution

<ol>
    <li>
        Compilation 
        <ul>
            <li>Client: <code>erl c(client).</code></li>
            <li>Server: <code>erl c(server).</code></li>
            <li>Middleware: <code>erl c(middleware).</code></li>
        </ul>
    </li>
    <br/>
    <li>
        Engine Execution
        <ul>
            <li>As Client: <code>middleware:initializeEngineAsClient(PortNumber, ServerIP).</code></li>
            <li>As Server: <code>middleware:initializeEngineAsServer(PortNumber).</code></li>
            <li>As Simulator: <code>middleware:initializeEngineAsSimulator(PortNumber, ServerIP, NumberOfUsers, NumberOfIterations).</code></li>
            <i>Example: middleware:initializeEngineAsSimulator(9000, "10.20.166.XX", 15, 10).</i>
        </ul>
    </li>
</ol>

****

## Testing Methodology

<ol>
    <strong>Non-Simulation</strong>
    <li>The application was tested using 4 clients and 1 server</li>
    <li>These clients are subscribed to one another in different combinations</li>
    <li>Each of this client sends out tweets and accurate reception of these tweets was verified</li>
    <li>Other actions such as unsubscribe/subscribe and tweet querying based on hashtags and mentions was performed</li>
</ol>
<ol>
    <strong>Simulation</strong>
    <li>The application was simulated for a range of clients from 10 to 150 clients</li>
    <li>The metrics were written to a file which was then used for inferring the performance for a given set of parameters</li>
    <li>Each of the simulated clients is subscribed to other simulated users via ZipF distribution</li>
    <li>The clients then tweet according the following rules</li>
    <table>
    <th>User Tweet Frequency & Subscribers</th>
    <th>Time between Tweet (milliseconds)</th>    
    <th>Time between Login/Logout (milliseconds)</th>
    <tr>
        <td>High</td>
        <td>7000</td>
        <td>10000</td>
    </tr>
    <tr>
        <td>Medium</td>
        <td>14000</td>
        <td>20000</td>
    </tr>
    <tr>
        <td>Low</td>
        <td>21000</td>
        <td>30000</td>
    </tr>
</table>
</ol>

****

## Testing Results

<h4>Subscribe</h3>
<img src="lib/src/assets/Subscribe.JPG"/>
<br/>
<h4>Unsubscribe</h3>
<img src="lib/src/assets/Unsubscribe.JPG"/>
<br/>
<h4>Hash Tag Query</h3>
<img src="lib/src/assets/Hash Tag Query.JPG"/>
<br/>
<h4>Mention Query</h3>
<img src="lib/src/assets/Mention Query.JPG"/>
<br/>

### Results of Simulations


<h4>Simulation - 20 Users</h3>
<br/>
<img src="lib/src/assets/Simulate 20.JPG"/>
<br/>
<br/>
<h4>Simulation - 60 Users</h3>
<br/>
<img src="lib/src/assets/Simulate 60.JPG"/>
<br/>
<br/>
<h4>Simulation - 80 Users</h3>
<br/>
<img src="lib/src/assets/Simulate 80.JPG"/>
<br/>
<br/>
<h4>Activity Stats</h3>
<br/>
<img src="lib/src/assets/Activity Stats.JPG"/>
<br/>
<i>Note: The System is capable of simulating 150 users and more but since this activity results in very high communication delays, we have included results for up to 80 users.<i/>


