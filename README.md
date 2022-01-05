# nflbigdatabowl2022

## The following is a competition submission to the NFL Big Data Bowl 2022: 

Injuries to kickoff returners in the NFL are among the most gruesome in the sport, and sadly we only need to look back a few weekends back for a reminder of just how serious these incidents can be. The injury to Kylin Hill (GB) [https://profootballtalk.nbcsports.com/2021/10/28/kylin-hill-jonathan-ward-carted-off-after-collision-on-kickoff-return/] is just the most recent of serious injuries on a play that the NFL adjusted the rulebook on a few years ago in an attempt to limit concussions. 

Regardless of these risks, teams continue to send returners flying out of their own end zones-including some league stars like Cee Dee Lamb (DAL) and Deebo Samuel (SF). Are teams accurately assessing the risk to their kick returners on these plays? Are their specific actions or patterns within player movement, positioning, or game script that greatly impact the risk of injury to kickoff and punt returners? 

This NFL Big Data Bowl 2022 Submission seeks to model relationships between NFL player tracking data, along with player information and game script data, and serious injuries to kickoff and punt returners. 

For this project I built a random forest machine learning model with an oversample on plays that ended in injury to the returner based off of play result data. My most interesting and valuable custom variables are a series of player tracking data summaries that turn many thousands of rows of tracking data into model ready classification and numeric statistics relating to returner movement, pivoting, and acceleration, along with a count of the number of defenders within two ranges of distance. 

This project and the resulting models were hampered by extremely limited player injury data which made these research questions outside the scope of the available data, in addition to limitations of my work time and technical modeling ability. However, I believe the process I started in this submission contains a valuable first pass at an actionable analytics result which would provide value to players, coaches, and the league office alike. It was an exciting task and a great first experience with modeling competitions and bigdatabowl. Thank you for sharing such deep and well curated datasets. 

-Tyler Sanders 

