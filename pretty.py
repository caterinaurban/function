import sys
import re

content = []
for line in sys.stdin:
    l = line.strip()
    if l != '':
        content.append(l)

# with open("result.log") as f:
#     content = f.readlines()
# # you may also want to remove whitespace characters like `\n` at the end of each line
# content = [x for x in content if x != '']

label_pattern = re.compile('(\[\s*\d+:\]):')


statements = []
properties = {}
prop = None
target = None


for l in content:
    if l == "Abstract Syntax:":
        target = statements
    elif l.startswith("Property: "):
        prop = {}
        properties[l[10:]] = prop
    elif label_pattern.match(l):
        target = []
        label = label_pattern.match(l).group(1)
        prop[label] = target
    elif target is None:
        print("Invalid Input Format")
        sys.exit(-1)
    else:
        target.append(l)



print("""

<!DOCTYPE html>
<html>
<head>
    <title>Bootstrap 3 Template</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">

    <!-- Bootstrap core CSS -->
    <link href="http://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.4/css/bootstrap.min.css" rel="stylesheet" media="screen">

    <!-- HTML5 shim and Respond.js IE8 support of HTML5 elements and media queries -->
    <!--[if lt IE 9]>
    <script src="http://cdnjs.cloudflare.com/ajax/libs/html5shiv/3.7.2/html5shiv.js"></script>
    <script src="http://cdnjs.cloudflare.com/ajax/libs/respond.js/1.4.2/respond.js"></script>
    <![endif]-->
</head>
<body>


<div class="container">
    <div class="row">
        <h1>Analysis Result</h1>
    </div>

    <div class="row">
""")

# Tab header ----------------------------------------------------------
print(""" <ul class="nav nav-tabs" role="tablist"> """)

for id,p in enumerate(properties.keys()):
    print("""
      <li role="presentation"><a href="#{0}" class="tab_button" id="tab_{0}" aria-controls="home" role="tab" data-toggle="tab">{1}</a></li>  
    """.format(id,p))

print(""" </ul> """)

# ------------------------------------------------------------------------


properties_formatted = {}

for k in properties:
    properties_formatted[k] = {}
    inv = properties[k]
    for l in inv:
        formatted_state = '<span class="label label-default" data-html="true" data-container="body" data-placement="top" data-toggle="tooltip" title="<div style=\'text-align:left;font-size:12pt;\'>' \
                          + "<br/>".join(inv[l]) + '</div>">' + l + '</span>'
        properties_formatted[k][l] = formatted_state


# print(properties_formatted)

# Tab content -------------------------------------------------------------

print(""" <div class="tab-content"> """)

for id,p in enumerate(properties.keys()):
    formatted_property = properties_formatted[p]
    print(""" <div role="tabpanel" class="tab-pane active" id="{0}"> """.format(id))
    print("""<pre>""")
    for s in statements:
        formatted_stmt = s
        for l in formatted_property:
            formatted_stmt = formatted_stmt.replace(l, formatted_property[l])
        print(formatted_stmt)
        # print(s)
    print("""</pre>""")
    print(""" </div > """)



print(""" </div > """)


print("""
    </div>
</div>




<!-- jQuery (necessary for Bootstrap's JavaScript plugins) -->
<script src="http://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.3/jquery.min.js"></script>
<!-- Include all compiled plugins (below), or include individual files as needed -->
<script src="http://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.4/js/bootstrap.min.js"></script>

<script>
      $(function () { 
          $('[data-toggle="tooltip"]').tooltip() 
        $('.tab_button').last().click()
      })
      
  </script>
</body>
</html>


""")



